# vim:fileencoding=utf-8

DATA_PATH = '../data'

WEBSITE_DATA_PATH = DATA_PATH + '/website/'
DB_FILE_PATH = WEBSITE_DATA_PATH + 'comic.db'
CRAWLER_SRC_PATH = WEBSITE_DATA_PATH + 'crawler_src'
BOOK_THUMBNAILS_PATH = WEBSITE_DATA_PATH + 'book_thumbnails'
SECRET_KEY_FILE_PATH = WEBSITE_DATA_PATH + 'secret_key.txt'

INGESTION_DATA_PATH = DATA_PATH + '/ingestion/'
BOOK_IMGS_PATH = INGESTION_DATA_PATH + 'book_imgs'

LOG_PATH = '../log'

DB_FILE_URI = 'sqlite:///' + DB_FILE_PATH