# vim:fileencoding=utf-8

import os, shutil
from flask import Blueprint, render_template, request, redirect, url_for, send_from_directory
from singleton import db
from models.book import Book
from models.crawler import Crawler
from werkzeug import secure_filename
import utils, constants

book_mgr = Blueprint('book_mgr', __name__)

@book_mgr.route('/')
def index():
    return render_template('book_mgr/index.html', books = Book.query.all())

@book_mgr.route('/create')
def create():
    crawler_names = [crawler.name for crawler in Crawler.query.all()]
    return render_template('book_mgr/create.html', crawler_names=crawler_names)

@book_mgr.route('/create', methods=['POST'])
@utils.require_login
def create_confirmed():
    book = Book(request.form['name'].strip(), request.form['directory_url'].strip(), request.form['crawler_name'].strip())
    book.description = request.form['description'].strip()
    os.makedirs(os.path.join(constants.BOOK_IMGS_PATH, book.name))
    file = request.files['thumbnail']
    if file:
        filename = utils.gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(constants.BOOK_THUMBNAILS_PATH, filename))
        book.thumbnail_filename = filename
    db.session.add(book)
    db.session.commit()
    return redirect(url_for('.index'))

@book_mgr.route('/edit/<name>')
def edit(name):
    book = Book.query.filter_by(name=name).first_or_404()
    crawler_names = [crawler.name for crawler in Crawler.query.all()]
    return render_template('book_mgr/edit.html', book = book, crawler_names=crawler_names)

@book_mgr.route('/edit/<name>', methods=['POST'])
@utils.require_login
def edit_confirmed(name):
    book = Book.query.filter_by(name=name).first_or_404()
    book.description = request.form['description'].strip()
    book.directory_url = request.form['directory_url'].strip()
    book.crawler_name = request.form['crawler_name'].strip()
    book.exclude_chapters = request.form['exclude_chapters'].strip()
    file = request.files['thumbnail']
    if file:
        filename = utils.gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(constants.BOOK_THUMBNAILS_PATH, filename))
        os.remove(os.path.join(constants.BOOK_THUMBNAILS_PATH, book.thumbnail_filename))
        book.thumbnail_filename = filename
    db.session.commit()
    return redirect(url_for('.index'))

@book_mgr.route('/delete/<name>')
@utils.require_login
def delete(name):
    book = Book.query.filter_by(name=name).first_or_404()
    return render_template('book_mgr/delete.html', book = book)

@book_mgr.route('/delete/<name>', methods=['POST'])
def delete_confirmed(name):
    book = Book.query.filter_by(name=name).first_or_404()
    os.remove(os.path.join(constants.BOOK_THUMBNAILS_PATH, book.thumbnail_filename))
    shutil.rmtree(os.path.join(constants.BOOK_IMGS_PATH, name))
    db.session.delete(book)
    db.session.commit()
    return redirect(url_for('.index'))

@book_mgr.route('/details/<name>')
def details(name):
    book = Book.query.filter_by(name=name).first_or_404()
    return render_template('book_mgr/details.html', book = book)

@book_mgr.route('/thumbnail/<name>')
def thumbnail(name):
    return send_from_directory(constants.BOOK_THUMBNAILS_PATH, name)