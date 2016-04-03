# vim:fileencoding=utf-8

import sys, os, shutil, datetime, getpass
from flask import redirect, url_for
from app import flask, db
from views.books import books
from views.book_mgr import book_mgr
from views.crawler_mgr import crawler_mgr
from views.account import account
from views.crawler_ctrl import crawler_ctrl
from models.account import Account
import constants, utils, crawler_schedule

@flask.route('/')
def index():
    return redirect(url_for('books.index'))

if __name__ == '__main__':

    flask.register_blueprint(books, url_prefix='/books')
    flask.register_blueprint(book_mgr, url_prefix='/book_mgr')
    flask.register_blueprint(crawler_mgr, url_prefix='/crawler_mgr')
    flask.register_blueprint(account, url_prefix='/account')
    flask.register_blueprint(crawler_ctrl, url_prefix='/crawler_ctrl')

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

            crawler_schedule.start()

            flask.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()
            flask.run(host='0.0.0.0', port=constants.PORT, debug = True)

        elif cmd == 'run_release':
            
            crawler_schedule.start()

            utils.add_file_handler_to_logger(flask.logger, constants.FLASK_LOG_FILE_PATH)
            flask.secret_key = file(constants.SECRET_KEY_FILE_PATH, 'r').read()
            flask.run(host='0.0.0.0', port=constants.PORT, threaded=True)

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