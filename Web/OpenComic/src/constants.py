# vim:fileencoding=utf-8

import os

DATA_PATH = os.path.join('..', 'data')

WEBSITE_DATA_PATH = os.path.join(DATA_PATH, 'website')
DB_FILE_PATH = os.path.join(WEBSITE_DATA_PATH, 'comic.db')
CRAWLER_SRC_PATH = os.path.join(WEBSITE_DATA_PATH, 'crawler_src')
BOOK_THUMBNAILS_PATH = os.path.join(WEBSITE_DATA_PATH, 'book_thumbnails')
SECRET_KEY_FILE_PATH = os.path.join(WEBSITE_DATA_PATH, 'secret_key.txt')

INGESTION_DATA_PATH = os.path.join(DATA_PATH, 'ingestion')
BOOK_IMGS_PATH = os.path.join(INGESTION_DATA_PATH, 'book_imgs')

LOG_PATH = os.path.join('..', 'log')

DB_FILE_URI = 'sqlite:///' + DB_FILE_PATH