# vim:fileencoding=utf-8

import hashlib
from singleton import db
import utils

class Account(db.Model):
    name = db.Column(db.String(32), primary_key = True)
    email = db.Column(db.String(96), nullable = False)
    password_hash = db.Column(db.String(64), nullable = False)
    salt = db.Column(db.String(32), nullable = False)

    def __init__(self, name, email, password):
        self.name = name
        self.email = email
        self.set_password(password)

    def set_password(self, password):
        self.salt = utils.gen_random_string()
        self.password_hash = hashlib.sha1(self.salt + ',' + password).hexdigest()

    def check_password(self, password):
        return self.password_hash == hashlib.sha1(self.salt + ',' + password).hexdigest()

    def __repr__(self):
        return '<Account %r>' % self.name