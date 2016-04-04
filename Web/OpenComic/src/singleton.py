# vim:fileencoding=utf-8

from datetime import timedelta
from flask import Flask, session
from flask_sqlalchemy import SQLAlchemy
import constants

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = constants.DB_FILE_URI
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['PERMANENT_SESSION_LIFETIME '] = timedelta(14)

@app.before_request
def make_session_permanent():
    session.permanent = True

db = SQLAlchemy(app)