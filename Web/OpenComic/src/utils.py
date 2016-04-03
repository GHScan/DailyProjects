# vim:fileencoding=utf-8

import uuid, os

def gen_unique_filename(origin_filename):
    return str(uuid.uuid4()).replace('-', '_') + os.path.splitext(origin_filename)[1]

def gen_random_string():
    return os.urandom(24).encode('base-64')