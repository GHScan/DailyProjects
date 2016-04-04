# vim:fileencoding=utf-8

import datetime
from singleton import db

class Crawler(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    author_account = db.Column(db.String(32), nullable = False)
    src_filename = db.Column(db.String(64), nullable = False)
    last_modified = db.Column(db.DateTime, nullable = False)
    error_message = db.Column(db.Text)

    def __init__(self, name, author_account, src_filename):
        self.name = name
        self.author_account = author_account
        self.src_filename = src_filename
        self.last_modified = datetime.datetime.now()

    def __repr__(self):
        return '<Crawler %r>' % self.name