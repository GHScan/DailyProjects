# vim:fileencoding=utf-8

import uuid, os

def gen_unique_filename(origin_filename):
    return str(uuid.uuid4()).replace('-', '_') + os.path.splitext(origin_filename)[1]