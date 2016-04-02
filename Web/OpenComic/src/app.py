# vim:fileencoding=utf-8

from flask import Flask
from flask_sqlalchemy import SQLAlchemy
import constants

flask = Flask(__name__)
flask.config['SQLALCHEMY_DATABASE_URI'] = constants.DB_FILE_URI
flask.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(flask)