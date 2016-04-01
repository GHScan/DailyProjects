# vim:fileencoding=utf-8

#---------------------------------------------
# globals
#---------------------------------------------
DATA_PATH = 'data'
DB_FILE_PATH = DATA_PATH + '/website/comic.db'
CRAWLER_SRCS_PATH = DATA_PATH + '/website/crawler_srcs'
BOOK_THUMBNAILS_PATH = DATA_PATH + '/website/book_thumbnails'
BOOK_IMGS_PATH = DATA_PATH + '/book_imgs'
LOGS_PATH = 'logs'
DB_FILE_URI = 'sqlite:///' + DB_FILE_PATH

#---------------------------------------------
# initialize
#---------------------------------------------
import datetime, os, sys, shutil
from flask import Flask, render_template, redirect, send_from_directory, request, url_for
from flask_sqlalchemy import SQLAlchemy
from werkzeug import secure_filename

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = DB_FILE_URI
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)

def gen_unique_filename(origin_filename):
    import uuid
    return str(uuid.uuid4()).replace('-', '_') + os.path.splitext(origin_filename)[1]
#---------------------------------------------
# models
#---------------------------------------------
class Crawler(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    author_email = db.Column(db.String(96), nullable = False)
    src_filename = db.Column(db.String(64), nullable = False)
    last_modified = db.Column(db.DateTime, nullable = False)

    def __init__(self, name, author_email, src_filename):
        self.name = name
        self.author_email = author_email
        self.src_filename = src_filename
        self.last_modified = datetime.datetime.now()

    def __repr__(self):
        return '<Crawler %r>' % self.name

class Book(db.Model):
    name = db.Column(db.String(64), primary_key = True)
    description = db.Column(db.String(64))
    thumbnail_filename = db.Column(db.String(64))
    directory_url = db.Column(db.Text, nullable = False)
    crawler_name = db.Column(db.String(32), nullable = False)

    def __init__(self, name, directory_url, crawler_name):
        self.name = name
        self.directory_url = directory_url
        self.crawler_name = crawler_name

    def __repr__(self):
        return '<Book %r>' % self.name        

#---------------------------------------------
# controllers
#---------------------------------------------

# controller_books
@app.route('/')
@app.route('/books')
def controller_books_index():
    return render_template('books_index.html', books = Book.query.all())

@app.route('/books/details/<name>')
def controller_books_details(name):
    root = os.path.join(BOOK_IMGS_PATH, name)
    chapters = [{ 
            'name': chapter_name, 
            'first_page' : [img_name for img_name in sorted(os.listdir(os.path.join(root, chapter_name))) 
                                if os.path.isfile(os.path.join(root, chapter_name, img_name))][0] } 
        for chapter_name in reversed(sorted(os.listdir(root))) if os.path.isdir(os.path.join(root, chapter_name))]
    return render_template('books_details.html', book={ 'name' : name, 'chapters' : chapters})

@app.route('/books/chapter/<book>/<name>')
def controller_books_chapter(book, name):
    root = os.path.join(BOOK_IMGS_PATH, book)
    chapters = [chapter_name for chapter_name in sorted(os.listdir(root)) if os.path.isdir(os.path.join(root, chapter_name))]
    chapter_index = chapters.index(name)
    prev_chapter = chapters[chapter_index - 1] if chapter_index > 0 else None
    next_chapter = chapters[chapter_index + 1] if chapter_index < len(chapters) - 1 else None

    root = os.path.join(BOOK_IMGS_PATH, book, name)
    img_names = [img_name for img_name in sorted(os.listdir(root)) if os.path.isfile(os.path.join(root, img_name))]

    return render_template('books_chapter.html', chapter=
        {'name' : name, 'book_name' : book, 'img_names' : img_names, 'prev_chapter' : prev_chapter, 'next_chapter' : next_chapter })

@app.route('/books/img/<book>/<chapter>/<name>')
def controller_books_img(book, chapter, name):
    return send_from_directory(os.path.join(BOOK_IMGS_PATH, book, chapter), name)

# controller_crawler_mgr
@app.route('/crawler_mgr')
def controller_crawler_mgr_index():
    return render_template('crawler_mgr_index.html', crawlers = Crawler.query.all())

@app.route('/crawler_mgr/create')
def controller_crawler_mgr_create():
    return render_template('crawler_mgr_create.html')

@app.route('/crawler_mgr/create', methods=['POST'])
def controller_crawler_mgr_create_confirmed():
    file = request.files['source']
    filename = gen_unique_filename(secure_filename(file.filename))
    file.save(os.path.join(CRAWLER_SRCS_PATH, filename))

    crawler = Crawler(request.form['name'].strip(), request.form['author_email'].strip(), filename)
    db.session.add(crawler)
    db.session.commit()
    return redirect(url_for('controller_crawler_mgr_index'))

@app.route('/crawler_mgr/edit/<name>')
def controller_crawler_mgr_edit(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.src_content = file(os.path.join(CRAWLER_SRCS_PATH, crawler.src_filename), 'r').read().decode('utf-8')
    return render_template('crawler_mgr_edit.html', crawler = crawler)

@app.route('/crawler_mgr/edit/<name>', methods=['POST'])
def controller_crawler_mgr_edit_confirmed(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.author_email = request.form['author_email'].strip()
    file = request.files['source']
    if file:
        filename = gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(CRAWLER_SRCS_PATH, filename))
        os.remove(os.path.join(CRAWLER_SRCS_PATH, crawler.src_filename))
        crawler.src_filename = filename
    crawler.last_modified = datetime.datetime.now()
    db.session.commit()
    return redirect(url_for('controller_crawler_mgr_index'))

@app.route('/crawler_mgr/delete/<name>')
def controller_crawler_mgr_delete(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    return render_template('crawler_mgr_delete.html', crawler = crawler)

@app.route('/crawler_mgr/delete/<name>', methods=['POST'])
def controller_crawler_mgr_delete_confirmed(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    os.remove(os.path.join(CRAWLER_SRCS_PATH, crawler.src_filename))
    db.session.delete(crawler)
    db.session.commit()
    return redirect(url_for('controller_crawler_mgr_index'))

@app.route('/crawler_mgr/details/<name>')
def controller_crawler_mgr_details(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.src_content = file(os.path.join(CRAWLER_SRCS_PATH, crawler.src_filename), 'r').read().decode('utf-8')
    return render_template('crawler_mgr_details.html', crawler = crawler)

# controller_book_mgr
@app.route('/book_mgr')
def controller_book_mgr_index():
    return render_template('book_mgr_index.html', books = Book.query.all())

@app.route('/book_mgr/create')
def controller_book_mgr_create():
    return render_template('book_mgr_create.html')

@app.route('/book_mgr/create', methods=['POST'])
def controller_book_mgr_create_confirmed():
    book = Book(request.form['name'].strip(), request.form['directory_url'].strip(), request.form['crawler_name'].strip())
    book.description = request.form['description'].strip()
    os.makedirs(os.path.join(BOOK_IMGS_PATH, book.name))
    file = request.files['thumbnail']
    if file:
        filename = gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(BOOK_THUMBNAILS_PATH, filename))
        book.thumbnail_filename = filename
    db.session.add(book)
    db.session.commit()
    return redirect(url_for('controller_book_mgr_index'))

@app.route('/book_mgr/edit/<name>')
def controller_book_mgr_edit(name):
    book = Book.query.filter_by(name=name).first_or_404()
    return render_template('book_mgr_edit.html', book = book)

@app.route('/book_mgr/edit/<name>', methods=['POST'])
def controller_book_mgr_edit_confirmed(name):
    book = Book.query.filter_by(name=name).first_or_404()
    book.description = request.form['description'].strip()
    book.directory_url = request.form['directory_url'].strip()
    book.crawler_name = request.form['crawler_name'].strip()
    file = request.files['thumbnail']
    if file:
        filename = gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(BOOK_THUMBNAILS_PATH, filename))
        os.remove(os.path.join(BOOK_THUMBNAILS_PATH, book.thumbnail_filename))
        book.thumbnail_filename = filename
    db.session.commit()
    return redirect(url_for('controller_book_mgr_index'))

@app.route('/book_mgr/delete/<name>')
def controller_book_mgr_delete(name):
    book = Book.query.filter_by(name=name).first_or_404()
    return render_template('book_mgr_delete.html', book = book)

@app.route('/book_mgr/delete/<name>', methods=['POST'])
def controller_book_mgr_delete_confirmed(name):
    book = Book.query.filter_by(name=name).first_or_404()
    os.remove(os.path.join(BOOK_THUMBNAILS_PATH, book.thumbnail_filename))
    shutil.rmtree(os.path.join(BOOK_IMGS_PATH, name))
    db.session.delete(book)
    db.session.commit()
    return redirect(url_for('controller_book_mgr_index'))

@app.route('/book_mgr/details/<name>')
def controller_book_mgr_details(name):
    book = Book.query.filter_by(name=name).first_or_404()
    return render_template('book_mgr_details.html', book = book)

@app.route('/book_mgr/thumbnail/<name>')
def controller_book_mgr_thumbnail(name):
    return send_from_directory(BOOK_THUMBNAILS_PATH, name)

#---------------------------------------------
# main
#---------------------------------------------
if len(sys.argv) <= 1:
    print """
    usage:
        run
        run_release
        db_create_all
    """
    exit(0)

else:
    if sys.argv[1] == 'run':
        app.run(host='0.0.0.0', port=80, debug = True)

    elif sys.argv[1] == 'run_release':
        import logging
        from logging.handlers import RotatingFileHandler
        file_handler = RotatingFileHandler(os.path.join(LOGS_PATH, 'flask.log'), maxBytes=1024 * 1024 * 10, backupCount=5)
        file_handler.setLevel(logging.ERROR)
        file_handler.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s\n\n"))
        app.logger.addHandler(file_handler)

        app.run(host='0.0.0.0', port=80, threaded=True)

    elif sys.argv[1] == 'setup':
        raw_input('press enter to terminate...')

        os.makedirs(BOOK_IMGS_PATH)
        os.makedirs(BOOK_THUMBNAILS_PATH)
        os.makedirs(CRAWLER_SRCS_PATH)
        os.makedirs(LOGS_PATH)
        db.create_all()

    elif sys.argv[1] == 'cleanup':
        raw_input('press enter to terminate...')

        db.drop_all()
        shutil.rmtree(DATA_PATH)
        shutil.rmtree(LOGS_PATH)

    elif sys.argv[1] == 'backup':
        shutil.make_archive(datetime.datetime.now().strftime("backup_%Y%m%d_%H%M%S"), 'zip', '.', 'data/website')