# vim:fileencoding=utf-8

import os

PORT = 80

DATA_PATH = os.path.join('..', 'data')

WEBSITE_DATA_PATH = os.path.join(DATA_PATH, 'website')
DB_FILE_PATH = os.path.join(WEBSITE_DATA_PATH, 'comic.db')
CRAWLER_SRC_PATH = os.path.join(WEBSITE_DATA_PATH, 'crawler_src')
BOOK_THUMBNAILS_PATH = os.path.join(WEBSITE_DATA_PATH, 'book_thumbnails')
SECRET_KEY_FILE_PATH = os.path.join(WEBSITE_DATA_PATH, 'secret_key.txt')

INGESTION_DATA_PATH = os.path.join(DATA_PATH, 'ingestion')
BOOK_IMGS_PATH = os.path.join(INGESTION_DATA_PATH, 'book_imgs')

LOG_PATH = os.path.join('..', 'log')
FLASK_LOG_FILE_PATH = os.path.join(LOG_PATH, 'flask.log')

DB_FILE_URI = 'sqlite:///' + DB_FILE_PATH

CHAPTERS_TO_KEEP = 10

CRAWL_MAX_RETRY = 3

BACKGROUND_CRAWL_INTERVAL_IN_MIN = 10