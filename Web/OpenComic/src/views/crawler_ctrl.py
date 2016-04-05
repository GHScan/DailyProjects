# vim:fileencoding=utf-8

import os, shutil, imp, traceback, sys
from multiprocessing import Process, Queue
from flask import Blueprint, request, jsonify, redirect, url_for
from models.book import Book
from models.crawler import Crawler
from singleton import db
import constants, utils

'''
    Crawler Specification 
        (unicode in memory, UTF-8 in file)

        - get_chapters(book_directory_url) -> [{name=chapter_name, url=chapter_url}]
        - download_chapter(chapter_url, chapter_directory_path) -> ()

'''
#----------------------------------------------------------------------------
def crawler_get_chapters(crawler, url):
    for i in xrange(sys.maxint):
        try:
            return crawler.get_chapters(url)
        except:
            if i == constants.CRAWL_MAX_RETRY - 1:
                raise

def crawler_download_chapter(crawler, chapter_url, chapter_directory_path):
    for i in xrange(sys.maxint):
        try:
            os.mkdir(chapter_directory_path)
            crawler.download_chapter(chapter_url, chapter_directory_path)
            return
        except:
            shutil.rmtree(chapter_directory_path)
            if i == constants.CRAWL_MAX_RETRY - 1:
                raise

def crawl_book_process(output_queue, crawler_source_path, directory_url, book_directory_path, exclude_chapters):
    try:
        crawler = imp.load_source('crawler', crawler_source_path) 

        net_chapters = [{'name':chapter['name'].strip(), 'url':chapter['url'].strip()} 
                for chapter in crawler_get_chapters(crawler, directory_url)]
        net_chapter_names = set(chapter['name'] for chapter in net_chapters)

        local_chapter_names = set(chapter_name for chapter_name in os.listdir(book_directory_path)
                            if os.path.isdir(os.path.join(book_directory_path, chapter_name)))

        exclude_chapter_names = set(name.strip() for name in exclude_chapters.split('\n'))

        final_chapter_names = set(list(reversed(sorted((local_chapter_names | net_chapter_names) - exclude_chapter_names)))[:constants.CHAPTERS_TO_KEEP])

        for chapter in net_chapters:
            if chapter['name'] in final_chapter_names and not chapter['name'] in local_chapter_names:

                crawler_download_chapter(crawler, chapter['url'], os.path.join(book_directory_path, chapter['name']))

        for chapter_name in local_chapter_names:
            if not chapter_name in final_chapter_names:
                shutil.rmtree(os.path.join(book_directory_path, chapter_name))


        output_queue.put(None)
    except:
        output_queue.put('book_directory_path=%s\nstack_trace=%s' % (book_directory_path, traceback.format_exc()))
#----------------------------------------------------------------------------        

crawler_ctrl = Blueprint('crawler_ctrl', __name__)

@crawler_ctrl.route('/books')
def books():
    return jsonify(books=[book.name for book in Book.query.all()])

@crawler_ctrl.route('/crawl_book/<name>', methods=['POST'])
@utils.require_login
def crawl_book(name):
    book = Book.query.filter_by(name=name).first_or_404()
    crawler = Crawler.query.filter_by(name=book.crawler_name).first_or_404()
    if crawler.error_message:
        return jsonify(result='failed', message='already error')

    q = Queue()
    p = Process(target=crawl_book_process, 
        args=(q, 
            os.path.join(constants.CRAWLER_SRC_PATH, crawler.src_filename),
            book.directory_url,
            os.path.join(constants.BOOK_IMGS_PATH, book.name),
            book.exclude_chapters))
    p.start()
    error_message = q.get()
    p.join()

    if error_message:
        crawler.error_message = error_message
        db.session.commit()
        return jsonify(result='failed', message='error')
    else:
        return jsonify(result='success')

@crawler_ctrl.route('/force_crawl_book/<name>', methods=['POST'])
@utils.require_login
def force_crawl_book(name):
    book = Book.query.filter_by(name=name).first_or_404()
    crawler = Crawler.query.filter_by(name=book.crawler_name).first_or_404()
    if crawler.error_message:
        crawler.error_message = None
        db.session.commit()
    return redirect(url_for('.crawl_book', name=name), code=307)
