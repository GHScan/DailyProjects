# vim:fileencoding=utf-8

from app import db

class Book(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    description = db.Column(db.String(96))
    thumbnail_filename = db.Column(db.String(64))
    directory_url = db.Column(db.Text, nullable = False)
    crawler_name = db.Column(db.String(32), nullable = False)

    def __init__(self, name, directory_url, crawler_name):
        self.name = name
        self.directory_url = directory_url
        self.crawler_name = crawler_name

    def __repr__(self):
        return '<Book %r>' % self.name