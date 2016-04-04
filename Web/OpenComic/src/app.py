# vim:fileencoding=utf-8

from datetime import timedelta
from flask import Flask, session
from flask_sqlalchemy import SQLAlchemy
import constants

flask = Flask(__name__)
flask.config['SQLALCHEMY_DATABASE_URI'] = constants.DB_FILE_URI
flask.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
flask.config['PERMANENT_SESSION_LIFETIME '] = timedelta(14)

@flask.before_request
def make_session_permanent():
    session.permanent = True

db = SQLAlchemy(flask)