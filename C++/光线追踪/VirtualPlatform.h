// vim: fileencoding=gbk

#ifndef VIRTUALPLATFORM_H
#define VIRTUALPLATFORM_H

extern void setupScene();
extern void onDrawBuffer(char *buf, int w, int h, int pitch);
extern void onKeyDown(int k);
extern void onKeyUp(int k);
extern void onMouseButtonDown(int btn, float x, float y);
extern void onMouseButtonUp(int btn, float x, float y);
extern void onMouseMove(float x, float y);
extern void onUpdate(float elapse);
extern void cleanupScene();

extern char* loadImage(const char *fname, void*(*fmalloc)(int), int &w, int &h);
extern bool saveImage(const char *fname, const char *buf, int w, int h, int pitch);

const int K_UP = 'W';
const int K_DOWN = 'S';
const int K_LEFT = 'A';
const int K_RIGHT = 'D';
const int MOUSE_LBUTTON = 1;
const int MOUSE_RBUTTON = 2;

#endif // #ifndef VIRTUALPLATFORM_H
