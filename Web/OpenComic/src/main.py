# vim:fileencoding=utf-8

import sys, os, shutil, datetime
from flask import redirect, url_for
from app import flask, db
from views.books import books
from views.book_mgr import book_mgr
from views.crawler_mgr import crawler_mgr
import constants

flask.register_blueprint(books, url_prefix='/books')
flask.register_blueprint(book_mgr, url_prefix='/book_mgr')
flask.register_blueprint(crawler_mgr, url_prefix='/crawler_mgr')

@flask.route('/')
def index():
    return redirect(url_for('books.index'))

if len(sys.argv) <= 1:
    print """
    usage:
        run
        run_release
        setup
        cleanup
        backup
    """
    exit(0)

else:
    cmd = sys.argv[1] 
    if cmd == 'run':

        flask.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()

        flask.run(host='0.0.0.0', port=80, debug = True)

    elif cmd == 'run_release':
        import logging
        from logging.handlers import RotatingFileHandler
        file_handler = RotatingFileHandler(os.path.join(constants.LOG_PATH, 'flask.log'), maxBytes=1024 * 1024 * 10, backupCount=5)
        file_handler.setLevel(logging.ERROR)
        file_handler.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s\n\n"))
        flask.logger.addHandler(file_handler)

        flask.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()

        flask.run(host='0.0.0.0', port=80, threaded=True)

    elif cmd == 'setup':
        raw_input('press any key to continue...')

        os.makedirs(constants.BOOK_IMGS_PATH)
        os.makedirs(constants.BOOK_THUMBNAILS_PATH)
        os.makedirs(constants.CRAWLER_SRC_PATH)
        os.makedirs(constants.LOG_PATH)
        db.create_all()

        file(constants.SECRET_KEY_FILE_PATH, 'w').write(os.urandom(24).encode('base-64'))

    elif cmd == 'cleanup':
        raw_input('press any key to continue...')

        db.drop_all()
        shutil.rmtree(constants.DATA_PATH)
        shutil.rmtree(constants.LOG_PATH)

    elif cmd == 'backup':
        shutil.make_archive(datetime.datetime.now().strftime("backup_%Y%m%d_%H%M%S"), 'zip', '.', constants.WEBSITE_DATA_PATH)
