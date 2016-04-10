# vim:fileencoding=utf-8

import os
from flask import Blueprint, render_template, send_from_directory, session
from models.book import Book
from models.read_history import ReadHistory
from singleton import db
import constants, utils

books = Blueprint('books', __name__)

@books.route('/')
def index():
    def _compare_book(a, b):
        if a.has_new == b.has_new:
            return -cmp(a.name, b.name)
        return -1 if a.has_new else 1

    account_name = session['account_name'] if 'account_name' in session else ''

    book_2_latest_read_chapter = { history.book: history.latest_chapter 
        for history in ReadHistory.query.filter_by(account = account_name).all() }

    books = Book.query.all()
    for book in books:
        root = os.path.join(constants.BOOK_IMGS_PATH, book.name)
        chapter_names = [chapter_name 
            for chapter_name in reversed(sorted(os.listdir(root))) if os.path.isdir(os.path.join(root, chapter_name))]

        book.has_new = len(chapter_names) > 0 and (
            book.name not in book_2_latest_read_chapter or chapter_names[0] > book_2_latest_read_chapter[book.name])

    books.sort(cmp=_compare_book)
        
    return render_template('books/index.html', books = books)

@books.route('/details/<name>')
def details(name):
    account_name = session['account_name'] if 'account_name' in session else ''
    
    history = ReadHistory.query.filter(
        ReadHistory.account==account_name, ReadHistory.book==name).first()
    latest_read_chapter = history.latest_chapter if history else ''

    root = os.path.join(constants.BOOK_IMGS_PATH, name)
    chapters = [{ 
            'name': chapter_name, 
            'first_page' : utils.first_or_default(
                    [img_name for img_name in sorted(os.listdir(os.path.join(root, chapter_name))) 
                        if os.path.isfile(os.path.join(root, chapter_name, img_name))], 
                    '_'),
            'is_new' : chapter_name > latest_read_chapter }
        for chapter_name in reversed(sorted(os.listdir(root))) if os.path.isdir(os.path.join(root, chapter_name))]

    return render_template('books/details.html', book={ 'name' : name, 'chapters' : chapters})

@books.route('/chapter/<book>/<name>')
def chapter(book, name):
    root = os.path.join(constants.BOOK_IMGS_PATH, book)
    chapters = [chapter_name for chapter_name in sorted(os.listdir(root)) if os.path.isdir(os.path.join(root, chapter_name))]
    chapter_index = chapters.index(name)
    prev_chapter = chapters[chapter_index - 1] if chapter_index > 0 else None
    next_chapter = chapters[chapter_index + 1] if chapter_index < len(chapters) - 1 else None

    root = os.path.join(constants.BOOK_IMGS_PATH, book, name)
    img_names = [img_name for img_name in sorted(os.listdir(root)) if os.path.isfile(os.path.join(root, img_name))]

    history = ReadHistory.query.filter(
        ReadHistory.account==session['account_name'], ReadHistory.book==book).first()
    if not history:
        history = ReadHistory(session['account_name'], book, name)
        db.session.add(history)
        db.session.commit()
    elif name > history.latest_chapter:
        history.latest_chapter = name
        db.session.commit()

    return render_template('books/chapter.html', chapter=
        {'name' : name, 'book_name' : book, 'img_names' : img_names, 'prev_chapter' : prev_chapter, 'next_chapter' : next_chapter })

@books.route('/img/<book>/<chapter>/<name>')
def img(book, chapter, name):
    return send_from_directory(os.path.join(constants.BOOK_IMGS_PATH, book, chapter), name)
