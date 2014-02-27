# vim:fileencoding=utf-8

import os
import web

urls = (
    ur'/', u'Help',
    ur'/help', u'Help',
    ur'/listdir', u'ListDir',
    ur'/hash', u'Hash',
    ur'/download', u'Download',
        )

class Help(object):
    def GET(self):
        web.header(u'Content-Type', u'text/plain; charset=utf-8', True)
        return u'''
        help:
        listdir: path
        hash: path, type={md5/sha1/...}
        download:
    '''

class ListDir(object):
    def GET(self):
        args = web.input()
        web.header(u'Content-Type', u'text/plain; charset=utf-8', True)
        pathlist = []
        for dirpath, dirnames, filenames in os.walk(args.path):
            pathlist += (os.path.join(dirpath, fname).encode(u'utf-8') for fname in filenames)
        return '\n'.join(pathlist)

class Hash(object):
    @staticmethod
    def getFileHash(path, hashType):
        return Hash.getDataHash(file(path, u'rb').read(), hashType)
    @staticmethod
    def getDataHash(data, hashType):
        import hashlib
        h = hashlib.new(hashType)
        h.update(data)
        return h.hexdigest()
    def GET(self):
        args = web.input()
        web.header(u'Content-Type', u'text/plain; charset=utf-8', True)
        return Hash.getFileHash(args.path, args.type)

class Download(object):
    def GET(self):
        import mimetypes, datetime
        args = web.input()
        web.header(u'Content-Type', mimetypes.guess_type(args.path), True)
        dt = datetime.datetime.fromtimestamp(os.path.getmtime(args.path))
        web.http.modified(etag=Hash.getDataHash((u'%s %s' % (dt, args.path)).encode(u'utf-8'), u'md5'))
        return file(args.path, u'rb').read()

if __name__ == u'__main__':
    app = web.application(urls, globals())
    app.run()
