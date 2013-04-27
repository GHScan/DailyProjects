from distutils.core import setup
import py2exe
setup(
        windows=['main.py'],
        options = {'py2exe': { 'dll_excludes': ['MSVCP90.dll']}},
        )
