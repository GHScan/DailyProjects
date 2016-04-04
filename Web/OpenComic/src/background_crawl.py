
import time, urllib2, urllib
from threading import Thread
from flask import json
import constants

running = False

def crawl_thread():
    time.sleep(1)

    while running:
        url = 'http://localhost:%s/crawler_ctrl/books' % constants.PORT
        books = json.loads(urllib2.urlopen(url).read())['books']

        for book in books:
            if not running: break

            url = ('http://localhost:%s/crawler_ctrl/crawl_book/' % constants.PORT) + urllib.quote(book.encode('utf-8'))
            response = json.loads(urllib2.urlopen(url, data="").read())

            if response['result'] == 'fatal':
                raise Exception('fatal error: book=%s, message=%s' % (book, response['message']))

        for _ in range(constants.BACKGROUND_CRAWL_INTERVAL_IN_MIN * 60):
            if not running: break
            time.sleep(1)

def start():
    global running
    running = True

    Thread(target = crawl_thread).start()

def stop():
    global running
    running = False