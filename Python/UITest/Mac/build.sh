#!/bin/bash

py2applet --arch=i386 --make-setup main.py
rm -rf build dist
python setup.py py2app
