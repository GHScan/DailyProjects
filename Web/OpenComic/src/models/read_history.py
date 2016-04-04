# vim:fileencoding=utf-8

import uuid
from singleton import db

class ReadHistory(db.Model):
    id = db.Column(db.String(48), primary_key=True)
    account = db.Column(db.String(32), nullable = False)
    book = db.Column(db.String(32), nullable = False)
    latest_chapter = db.Column(db.String(128), nullable = False)

    def __init__(self, account, book, latest_chapter):
        self.id = str(uuid.uuid4())
        self.account = account
        self.book = book
        self.latest_chapter = latest_chapter

    def __repr__(self):
        return '<ReadHistory %r, %r>' % (self.account, self.book)