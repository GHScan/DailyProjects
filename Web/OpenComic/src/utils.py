# vim:fileencoding=utf-8

import uuid, os, functools
from flask import session, Response

def gen_unique_filename(origin_filename):
    return str(uuid.uuid4()).replace('-', '_') + os.path.splitext(origin_filename)[1]

def gen_random_string():
    return os.urandom(24).encode('base-64')

def require_login(f):
    @functools.wraps(f)
    def decorated(*args, **kwargs):
        if not 'account_name' in session:
            return Response(
                'Could not verify your access level for that URL.\n'
                'You have to login with proper credentials', 401,
                {'WWW-Authenticate': 'Basic realm="Login Required"'})
        return f(*args, **kwargs)
    return decorated