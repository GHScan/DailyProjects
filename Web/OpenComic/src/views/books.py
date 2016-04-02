# vim:fileencoding=utf-8

import os
from flask import Blueprint, render_template, send_from_directory
from models.book import Book
import constants

books = Blueprint('books', __name__)

@books.route('/')
def index():
    return render_template('books/index.html', books = Book.query.all())

@books.route('/details/<name>')
def details(name):
    root = os.path.join(constants.BOOK_IMGS_PATH, name)
    chapters = [{ 
            'name': chapter_name, 
            'first_page' : [img_name for img_name in sorted(os.listdir(os.path.join(root, chapter_name))) 
                                if os.path.isfile(os.path.join(root, chapter_name, img_name))][0] } 
        for chapter_name in reversed(sorted(os.listdir(root))) if os.path.isdir(os.path.join(root, chapter_name))]
    return render_template('books/details.html', book={ 'name' : name, 'chapters' : chapters})

@books.route('chapter/<book>/<name>')
def chapter(book, name):
    root = os.path.join(constants.BOOK_IMGS_PATH, book)
    chapters = [chapter_name for chapter_name in sorted(os.listdir(root)) if os.path.isdir(os.path.join(root, chapter_name))]
    chapter_index = chapters.index(name)
    prev_chapter = chapters[chapter_index - 1] if chapter_index > 0 else None
    next_chapter = chapters[chapter_index + 1] if chapter_index < len(chapters) - 1 else None

    root = os.path.join(constants.BOOK_IMGS_PATH, book, name)
    img_names = [img_name for img_name in sorted(os.listdir(root)) if os.path.isfile(os.path.join(root, img_name))]

    return render_template('books/chapter.html', chapter=
        {'name' : name, 'book_name' : book, 'img_names' : img_names, 'prev_chapter' : prev_chapter, 'next_chapter' : next_chapter })

@books.route('img/<book>/<chapter>/<name>')
def img(book, chapter, name):
    return send_from_directory(os.path.join(constants.BOOK_IMGS_PATH, book, chapter), name)