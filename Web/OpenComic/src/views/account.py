# vim:fileencoding=utf-8

import os
from flask import Blueprint, render_template, redirect, url_for, request, session
from singleton import db
from models.account import Account
import constants, utils

account = Blueprint('account', __name__)

@account.route('/login', methods=['POST'])
def login():
    name = request.form['name'].strip()
    account = Account.query.filter_by(name=name).first_or_404()
    if account.check_password(request.form['password'].strip()):
        session['account_name'] = name
        return redirect(url_for('books.index'))
    else:
        return 'Invalid password! Please try it again'

@account.route('/logout', methods=['POST'])
@utils.require_login
def logout():
    session.pop('account_name', None)
    return redirect(url_for('books.index'))

@account.route('/edit')
def edit():
    account = Account.query.filter_by(name=session['account_name']).first_or_404()
    return render_template('account/edit.html', account = account)

@account.route('/edit', methods=['POST'])
@utils.require_login
def edit_confirmed():
    account = Account.query.filter_by(name=session['account_name']).first_or_404()
    account.email = request.form['email'].strip()
    password = request.form['password'].strip()
    if password:
        account.set_password(password)
    db.session.commit()
    return redirect(url_for('books.index'))