#! /usr/bin/env python

import os, sys
import urllib2, cgi
import cStringIO
import SimpleHTTPServer, BaseHTTPServer

class Handler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    def do_GET(self):
        fsPath = self._getFSPath(urllib2.unquote(self.path))
        if os.path.isdir(fsPath):
            self._renderDir(fsPath)
        else:
            if os.path.isfile(fsPath):
                self._renderFile(fsPath)
            else:
                self.send_error(404)

    def _getFSPath(self, path):
        return os.path.join(*([os.getcwd()] + path.split('/')))
    def _getURLPath(self, path):
        return path[len(os.getcwd()):] or '/'

    def _renderFile(self, path):
        self.send_response(200)
        self.send_header('content-type', self.guess_type(path))
        self.send_header('content-length', os.path.getsize(path))
        self.send_header('last-modified', self.date_time_string(os.path.getmtime(path)))
        self.end_headers()
        self.copyfile(file(path, 'rb'), self.wfile)

    def _renderDir(self, path):
        f = cStringIO.StringIO()

        f.write('<html>')
        f.write('<title>listing %s</title>' % cgi.escape(path))
        f.write('<body>')
        f.write('<h1>listing %s</h1>' % cgi.escape(path))

        items = []  
        if self._getURLPath(path) != '/': items.append(('..', os.path.split(path)[0]))
        items += ((fname, os.path.join(path, fname)) for fname in sorted(os.listdir(path)))
        f.write('<ul>')
        for fname, npath in items:
            f.write('<li><a href="%s">%s</a></li>' % (urllib2.quote(self._getURLPath(npath)), cgi.escape(fname)))
        f.write('</ul>')

        f.write('</body>')
        f.write('</html>')
        length = f.tell()
        f.seek(0)

        self.send_response(200)
        self.send_header('content-type', 'text/html; charset=%s' % sys.getfilesystemencoding())
        self.send_header('content-length', length)
        self.send_header('last-modified', self.date_time_string())
        self.end_headers()
        self.copyfile(f, self.wfile)

server = BaseHTTPServer.HTTPServer(('', 8000), Handler)
server.serve_forever()
