#! /usr/bin/env python
# coding=utf-8
import OpenGL 
OpenGL.ERROR_ON_COPY = True 
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from OpenGL.GL.shaders import *

import Image
import numpy
import math

import time, sys
program = None

points = [(0.5,0.5), (-0.5,0.5), (-0.5,-0.5), (0.5,-0.5)]
pointsUV = [(1, 0), (0, 0), (0, 1), (1, 1)]
winSize = (640, 480)
pickedPoint = -1

def InitGL():               
    glClearColor(1.0, 1.0, 1.0, 1.0)    
    glClearDepth(1.0)                   
    glShadeModel(GL_SMOOTH)          
    glEnable(GL_TEXTURE_2D)
    loadTextures()

    if not glUseProgram:
        print 'Missing Shader Objects!'
        sys.exit(1)
    global program
    program = compileProgram(
        compileShader(file('vs.glsl').read() ,GL_VERTEX_SHADER),
        compileShader(file('fs.glsl').read() ,GL_FRAGMENT_SHADER))

def drawGLScene():
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    if program:
        glUseProgram(program)

    # FIXME: use 4 instead of 8 in windows, because of the different implemention of PyOpenGL
    glUniform2fv(glGetUniformLocation(program, 'g_points'), 8, numpy.array(points, 'f'))
    glUniform2fv(glGetUniformLocation(program, 'g_uvs'), 8, numpy.array(pointsUV, 'f'))

    glBegin(GL_TRIANGLES)
    glTexCoord2f(pointsUV[2][0], pointsUV[2][1]); glVertex2f(points[2][0], points[2][1])
    glTexCoord2f(pointsUV[0][0], pointsUV[0][1]); glVertex2f(points[0][0], points[0][1])
    glTexCoord2f(pointsUV[1][0], pointsUV[1][1]); glVertex2f(points[1][0], points[1][1])
    glTexCoord2f(pointsUV[2][0], pointsUV[2][1]); glVertex2f(points[2][0], points[2][1])
    glTexCoord2f(pointsUV[3][0], pointsUV[3][1]); glVertex2f(points[3][0], points[3][1])
    glTexCoord2f(pointsUV[0][0], pointsUV[0][1]); glVertex2f(points[0][0], points[0][1])
    glEnd()

    glutSwapBuffers()

def winPos2scenePos(x, y):
    return (float(x) / winSize[0] * 2 - 1, 1 - float(y) / winSize[1] * 2)
def scenePos2winPos(x, y):
    return ((x + 1) / 2 * winSize[0], (1 - y) / 2 * winSize[1])
def distant(p1, p2):
    dx, dy = p1[0] - p2[0], p1[1] - p2[1]
    return math.sqrt(dx * dx + dy * dy)
def choosePoints(x, y):
    for i, pt in enumerate(points):
        if distant(scenePos2winPos(*pt), (x,y)) < 10:
            return i
    return -1

def keyPressed(*args):
    if args[0] == '\x1b': sys.exit()
def mouseEvent(btn, state, x, y):
    global pickedPoint
    if btn == 0:
        if state == 0:
            pickedPoint = choosePoints(x, y)
        elif state == 1:
            pickedPoint = -1
def mouseMotionEvent(x, y):
    if pickedPoint != -1:
        points[pickedPoint] = winPos2scenePos(x, y)
        glutPostRedisplay()

def loadTextures():
    image = Image.open("mirrow2.bmp")
	
    ix = image.size[0]
    iy = image.size[1]
    image = image.tostring("raw", "RGBX", 0, -1)
	
    glBindTexture(GL_TEXTURE_2D, glGenTextures(1))
	
    glPixelStorei(GL_UNPACK_ALIGNMENT,1)
    glTexImage2D(GL_TEXTURE_2D, 0, 3, ix, iy, 0, GL_RGBA, GL_UNSIGNED_BYTE, image)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)


glutInit(sys.argv)
glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE)
glutInitWindowSize(*winSize)
glutInitWindowPosition(300, 300)
window = glutCreateWindow('Shader 2D')
glutDisplayFunc(drawGLScene)
glutKeyboardFunc(keyPressed)
glutMouseFunc(mouseEvent)
glutMotionFunc(mouseMotionEvent)
InitGL()
glutMainLoop()
