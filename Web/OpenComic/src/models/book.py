# vim:fileencoding=utf-8

from singleton import db

class Book(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    description = db.Column(db.String(96))
    thumbnail_filename = db.Column(db.String(64))
    directory_url = db.Column(db.Text, nullable = False)
    crawler_name = db.Column(db.String(32), nullable = False)
    exclude_chapters = db.Column(db.Text, nullable = False)

    def __init__(self, name, directory_url, crawler_name):
        self.name = name
        self.directory_url = directory_url
        self.crawler_name = crawler_name
        self.exclude_chapters = ''

    def __repr__(self):
        return '<Book %r>' % self.name