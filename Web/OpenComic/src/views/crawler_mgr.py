# vim:fileencoding=utf-8

import os, datetime
from flask import Blueprint, render_template, request, redirect, url_for, session
from singleton import db
from models.crawler import Crawler
from werkzeug import secure_filename
import utils, constants

crawler_mgr = Blueprint('crawler_mgr', __name__)

@crawler_mgr.route('/')
def index():
    return render_template('crawler_mgr/index.html', crawlers = Crawler.query.all())

@crawler_mgr.route('/create')
def create():
    return render_template('crawler_mgr/create.html')

@crawler_mgr.route('/create', methods=['POST'])
@utils.require_login
def create_confirmed():
    file = request.files['source']
    filename = utils.gen_unique_filename(secure_filename(file.filename))
    file.save(os.path.join(constants.CRAWLER_SRC_PATH, filename))

    crawler = Crawler(request.form['name'].strip(), session['account_name'], filename)
    db.session.add(crawler)
    db.session.commit()
    return redirect(url_for('.index'))

@crawler_mgr.route('/edit/<name>')
def edit(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.src_content = file(os.path.join(constants.CRAWLER_SRC_PATH, crawler.src_filename), 'r').read().decode('utf-8')
    return render_template('crawler_mgr/edit.html', crawler = crawler)

@crawler_mgr.route('/edit/<name>', methods=['POST'])
@utils.require_login
def edit_confirmed(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.author_account = session['account_name']
    file = request.files['source']
    if file:
        filename = utils.gen_unique_filename(secure_filename(file.filename))
        file.save(os.path.join(constants.CRAWLER_SRC_PATH, filename))
        os.remove(os.path.join(constants.CRAWLER_SRC_PATH, crawler.src_filename))
        crawler.src_filename = filename
    crawler.last_modified = datetime.datetime.now()
    crawler.error_message = None
    db.session.commit()
    return redirect(url_for('.index'))

@crawler_mgr.route('/delete/<name>')
def delete(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    return render_template('crawler_mgr/delete.html', crawler = crawler)

@crawler_mgr.route('/delete/<name>', methods=['POST'])
@utils.require_login
def delete_confirmed(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    os.remove(os.path.join(constants.CRAWLER_SRC_PATH, crawler.src_filename))
    db.session.delete(crawler)
    db.session.commit()
    return redirect(url_for('.index'))

@crawler_mgr.route('/details/<name>')
def details(name):
    crawler = Crawler.query.filter_by(name=name).first_or_404()
    crawler.src_content = file(os.path.join(constants.CRAWLER_SRC_PATH, crawler.src_filename), 'r').read().decode('utf-8')
    return render_template('crawler_mgr/details.html', crawler = crawler)