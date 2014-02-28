# vim:fileencoding=utf-8

from __future__ import unicode_literals
import os
import web

urls = (
    r'/', 'Help',
    r'/help', 'Help',
    r'/listdir', 'ListDir',
    r'/hash', 'Hash',
    r'/download', 'Download',
        )

class Help(object):
    def GET(self):
        web.header('Content-Type', 'text/plain; charset=utf-8', True)
        return '''
        help:
        listdir: path
        hash: path, type={md5/sha1/...}
        download:
    '''

class ListDir(object):
    def GET(self):
        args = web.input()
        web.header('Content-Type', 'text/plain; charset=utf-8', True)
        pathlist = []
        for dirpath, dirnames, filenames in os.walk(args.path):
            pathlist += (os.path.join(dirpath, fname).encode('utf-8') for fname in filenames)
        return b'\n'.join(pathlist)

class Hash(object):
    @staticmethod
    def getFileHash(path, hashType):
        return Hash.getDataHash(file(path, 'rb').read(), hashType)
    @staticmethod
    def getDataHash(data, hashType):
        import hashlib
        h = hashlib.new(hashType)
        h.update(data)
        return h.hexdigest()
    def GET(self):
        args = web.input()
        web.header('Content-Type', 'text/plain; charset=utf-8', True)
        return Hash.getFileHash(args.path, args.type)

class Download(object):
    def GET(self):
        import mimetypes, datetime
        args = web.input()
        web.header('Content-Type', mimetypes.guess_type(args.path), True)
        web.header('Transform-encoding', 'Chunked', True)
        dt = datetime.datetime.fromtimestamp(os.path.getmtime(args.path))
        web.http.modified(etag=Hash.getDataHash(('%s %s' % (dt, args.path)).encode('utf-8'), 'md5'))

        BUF_SIZE = 2**20
        with file(args.path, 'rb') as f:
            while True:
                data = f.read(BUF_SIZE)
                if not data: break
                yield data

if __name__ == '__main__':
    app = web.application(urls, globals())
    app.run()
