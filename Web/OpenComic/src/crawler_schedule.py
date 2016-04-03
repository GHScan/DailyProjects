
import time, urllib2, urllib
from threading import Thread
from flask import json
import constants

def schedule_thread():
    while True:
        url = 'http://localhost:%s/crawler_ctrl/books' % constants.PORT
        books = json.loads(urllib2.urlopen(url).read())['books']

        for book in books:
            book = book.encode('utf-8')

            url = ('http://localhost:%s/crawler_ctrl/crawl_book/' % constants.PORT) + urllib.quote(book)
            response = json.loads(urllib2.urlopen(url, data="").read())
            result = response['result'].encode('utf-8')
            message = response['message'].encode('utf-8')

            if result == 'fatal':
                raise Exception('fatal error: book=%s, message=%s' % (book, message))

        time.sleep(constants.CRAWLER_SCHEDULE_INTERVAL_IN_MIN * 60)

def start():
    Thread(target = schedule_thread).start()