#! /usr/bin/env python
# vim:fileencoding=utf-8

import os, sys
import urllib2, mimetypes, datetime
import web

urls = (
        r'/', 'Index',
        r'/book/(\d+)', 'Book',
        r'/file/(.+)', 'File',
        )

class Index(object):
    bookpaths = sorted(os.path.join(os.getcwd(), fname) for fname in os.listdir(os.getcwd()))

    def GET(self):
        books = [{'name':os.path.split(path)[1], 'url':'/book/%s' % i} for i, path in enumerate(self.bookpaths)]

        template = '''$def with (name, books)
        <html>
        <head>
            <meta http-equiv=content-type content="text/html;charset=%s">
            <title>$name</title>
        </head>
        <body>
            <h1>$name</h1>
            <ul>
            $for book in books:
                <li>
                    <a href="$book['url']">$book['name']</a>
                </li>
            </ul>
        </body>
        </html>''' % sys.getfilesystemencoding()
        return web.template.Template(template)(os.path.split(os.getcwd())[1], books)

class Book(object):
    def GET(self, idx):
        idx = int(idx)
        path = Index.bookpaths[idx]

        bookInfo = {'name':os.path.split(path)[1]}
        if idx > 0: 
            bookInfo['prevUrl'] = '/book/%s' % (idx - 1)
        if idx + 1 < len(Index.bookpaths): 
            bookInfo['nextUrl'] = '/book/%s' % (idx + 1)

        images = ['/file/' + urllib2.quote(os.path.join(path, fname), '') for fname in sorted(os.listdir(path))]

        template = '''$def with (bookInfo, images)
        <html>
        <head>
            <meta http-equiv=content-type content="text/html;charset=%s">
            <title>$bookInfo['name']</title>
        </head>
        <body>
            <h1>$bookInfo['name']</h1>
            <hr/>
                $for imgUrl in images:
                    <img src="$imgUrl"/>
                    <hr/>
            <hr/>
                $if 'prevUrl' in bookInfo:
                    <a href="$bookInfo['prevUrl']">上一话</a>
                <a href="/">目录</a>
                $if 'nextUrl' in bookInfo:
                    <a href="$bookInfo['nextUrl']">下一话</a>
            <hr/>
        </body>
        </html>'''.decode('utf-8').encode(sys.getfilesystemencoding()) % sys.getfilesystemencoding()
        return web.template.Template(template)(bookInfo, images)

class File(object):
    def GET(self, path):
        path = urllib2.unquote(path)
        datet = datetime.datetime.fromtimestamp(os.path.getmtime(path))
        web.http.modified(date=datet)
        web.header('content-type', mimetypes.guess_type(path))
        return file(path, 'rb').read()

if __name__ == "__main__":
    app = web.application(urls, globals())
    app.run()
