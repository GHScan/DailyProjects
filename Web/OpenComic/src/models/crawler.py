# vim:fileencoding=utf-8

import datetime
from app import db

class Crawler(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    author_email = db.Column(db.String(96), nullable = False)
    src_filename = db.Column(db.String(64), nullable = False)
    last_modified = db.Column(db.DateTime, nullable = False)

    def __init__(self, name, author_email, src_filename):
        self.name = name
        self.author_email = author_email
        self.src_filename = src_filename
        self.last_modified = datetime.datetime.now()

    def __repr__(self):
        return '<Crawler %r>' % self.name