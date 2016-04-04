# vim:fileencoding=utf-8

import sys, os, shutil, datetime, getpass
from flask import redirect, url_for
from singleton import app, db
from views.books import books
from views.book_mgr import book_mgr
from views.crawler_mgr import crawler_mgr
from views.account import account
from views.crawler_ctrl import crawler_ctrl
from models.account import Account
import constants, utils, background_crawl

@app.route('/')
def index():
    return redirect(url_for('books.index'))

if __name__ == '__main__':

    app.register_blueprint(books, url_prefix='/books')
    app.register_blueprint(book_mgr, url_prefix='/book_mgr')
    app.register_blueprint(crawler_mgr, url_prefix='/crawler_mgr')
    app.register_blueprint(account, url_prefix='/account')
    app.register_blueprint(crawler_ctrl, url_prefix='/crawler_ctrl')

    if len(sys.argv) <= 1:
        print """
        usage:
            run
            run_release
            setup
            cleanup
            backup
            add_account name email
            sql_query query
        """
        exit(0)

    else:
        cmd = sys.argv[1] 
        if cmd == 'run':

            background_crawl.start()

            app.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()
            app.run(host='0.0.0.0', port=constants.PORT, debug = True)

            background_crawl.stop()

        elif cmd == 'run_release':
            
            background_crawl.start()

            utils.add_file_handler_to_logger(app.logger, constants.FLASK_LOG_FILE_PATH)
            app.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()
            app.run(host='0.0.0.0', port=constants.PORT, threaded=True)

            background_crawl.stop()

        elif cmd == 'setup':
            raw_input('press any key to continue...')

            os.makedirs(constants.BOOK_IMGS_PATH)
            os.makedirs(constants.BOOK_THUMBNAILS_PATH)
            os.makedirs(constants.CRAWLER_SRC_PATH)
            os.makedirs(constants.LOG_PATH)
            db.create_all()

            file(constants.SECRET_KEY_FILE_PATH, 'w').write(utils.gen_random_string())

        elif cmd == 'cleanup':
            raw_input('press any key to continue...')

            db.drop_all()
            shutil.rmtree(constants.DATA_PATH)
            shutil.rmtree(constants.LOG_PATH)

        elif cmd == 'backup':
            shutil.make_archive(datetime.datetime.now().strftime("backup_%Y%m%d_%H%M%S"), 'zip', '.', constants.WEBSITE_DATA_PATH)

        elif cmd == 'add_account':
            account = Account(sys.argv[2], sys.argv[3], getpass.getpass())
            db.session.add(account)
            db.session.commit()

        elif cmd == 'sql_query':
            print db.engine.execute(sys.argv[2]).fetchall()

        else:
            print 'invalid command:', sys.argv[1]
            exit(1)