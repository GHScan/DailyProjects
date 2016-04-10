# vim:fileencoding=utf-8

import uuid, os, functools, logging
from logging.handlers import RotatingFileHandler
from flask import session, Response, request

def gen_unique_filename(origin_filename):
    return str(uuid.uuid4()).replace('-', '_') + os.path.splitext(origin_filename)[1]

def gen_random_string():
    return os.urandom(24).encode('base-64')

def require_login(f):
    @functools.wraps(f)
    def decorated(*args, **kwargs):
        if request.remote_addr != '127.0.0.1' and not 'account_name' in session:
            return Response(
                'Could not verify your access level for that URL.\n'
                'You have to login with proper credentials', 401,
                {'WWW-Authenticate': 'Basic realm="Login Required"'})
        return f(*args, **kwargs)
    return decorated

def add_file_handler_to_logger(logger, file_path):
    file_handler = RotatingFileHandler(file_path, maxBytes=1024 * 1024 * 10, backupCount=5)
    file_handler.setLevel(logging.ERROR)
    file_handler.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s\n\n"))
    logger.addHandler(file_handler)

def first_or_default(a, default):
    for v in a:
        return v
    return default
