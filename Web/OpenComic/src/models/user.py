# vim:fileencoding=utf-8

import hashlib
from app import db
import utils

class User(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    email = db.Column(db.String(96), nullable = False)
    password_hash = db.Column(db.String(64), nullable = False)
    salt = db.Column(db.String(32), nullable = False)

    def __init__(self, name, email, password):
        self.name = name
        self.email = email
        self.setPassword(password)

    def setPassword(self, password):
        self.salt = utils.gen_random_string()
        self.password_hash = hashlib.sha1(self.salt + ',' + password).hexdigest()

    def checkPassword(self, password):
        return self.password_hash == hashlib.sha1(self.salt + ',' + password).hexdigest()

    def __repr__(self):
        return '<User %r>' % self.name