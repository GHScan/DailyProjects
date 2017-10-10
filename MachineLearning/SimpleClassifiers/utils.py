#vim:fileencoding=utf-8


import sys
import random
import numpy as np


def int_to_vector(i, max_i):
    v = np.zeros(max_i)
    v[i] = 1
    return v

def ext_dim(X):
    return np.expand_dims(X, axis=-1)


def identity(X):
    return X

def didentity(dY, Y, X):
    return dY


def sigmoid(X):
    return 1 / (1 + np.exp(-X))

def dsigmoid(dY, Y, X):
    return dY * Y * (1 - Y)


def reLU(X):
    return np.maximum(0, X)

def dreLU(dY, Y, X):
    dX = dY.copy()
    dX[X < 0] = 0
    return dX


def softmax(X):
    E = np.exp(X - ext_dim(np.max(X, axis=-1)))
    return E / ext_dim(np.sum(E, axis=-1))

def dsoftmax(dY, Y, X):
    return dY * Y * (1 - Y)


def ddot0(dY, Y, X, W):
    return np.dot(dY, W.T)

def ddot1(dY, Y, X, W):
    return np.dot(X.T, dY)


def l1_norm(X):
    return np.sum(np.abs(X), axis=-1)

def dl1_norm(dY, Y, X):
    dX = dY.repeat(X.shape[-1])
    dX = dX.reshape(X.shape)
    neg_idx = X < 0
    dX[neg_idx] = -dX[neg_idx]
    return dX

def l2_norm(X):
    return sqrl2_norm(X) ** 0.5

def sqrl2_norm(X):
    return np.sum(X ** 2, axis=-1)

def dsqrl2_norm(dY, Y, X):
    dX = dY.repeat(X.shape[-1])
    dX = dX.reshape(X.shape)
    dX = dX * 2 * X
    return dX


def mean_square_loss(Y, Y_expect):
    return sqrl2_norm(Y - Y_expect) / Y.shape[-1]

def dmean_square_loss(loss, Y, Y_expect):
    return 2 / Y.shape[-1] * (Y - Y_expect)


def cross_entropy_loss(Y, Y_expect):
    return np.sum(-np.log(Y) * Y_expect, axis=-1)

def dcross_entropy_loss(loss, Y, Y_expect):
    return -Y_expect / Y


def softmax_cross_entropy_loss(Y, Y_expect):
    Y = Y - ext_dim(np.max(Y, axis=-1))
    return np.log(np.sum(np.exp(Y), axis=-1)) - np.sum(Y * Y_expect, axis=-1)

def dsoftmax_cross_entropy_loss(loss, Y, Y_expect):
    return softmax(Y) - Y_expect


def dropout(X, prob):
    Y = X.copy()
    Y[np.random.rand(X.shape) < prob] = 0
    return Y

def ddropout(dY, Y, X):
    dX = dY.copy()
    dX[Y == 0] = 0
    return dX


def numerical_gradient(f, x, delta=0.001):
      grad = np.zeros(x.shape)

      fx = f(x) 
      it = np.nditer(x, flags=['multi_index'], op_flags=['readwrite'])
      while not it.finished:

        ix = it.multi_index
        old_value = x[ix]
        x[ix] = old_value + delta
        fxh = f(x) 
        x[ix] = old_value 

        grad[ix] = (fxh - fx) / delta
        it.iternext() 

      return grad


def normalize_data(X):
    return (X - np.mean(X)) / np.std(X)


def manhattan_dist(x0, x1):
    return l1_norm(x0 - x1)

def euclidean_dist(x0, x1):
    return l2_norm(x0 - x1)


def break_into_batches(X, batch_size, shuffle=False):
    idx = list(range(len(X)))
    if shuffle:
        random.shuffle(idx)

    l = []
    for off in range(0, len(idx), batch_size):
        l.clear()
        for i in idx[off:off + batch_size]:
            l.append(X[i])
        yield l


def lookup_dfunc(f):
    module = sys.modules[f.__module__]
    return getattr(module, 'd' + f.__name__)


def sgd_optimizer(params, grads, sample_count, batch_size, learning_rate, weight_decay):
    for param, grad in zip(params, grads):
        param *= 1 - learning_rate * weight_decay / np.ceil(sample_count / batch_size)
        param -= learning_rate / sample_count * grad


def step_learning_rate_scheduler(init_learning_rate, width, scale):
    def func_(epoch):
        return init_learning_rate * np.power(scale, epoch // width)
    return func_

def gaussian_weights_intializer(mean, std):
    def func_(shape):
        return np.random.randn(*shape) * (std / np.sqrt(shape[-1])) + mean
    return func_


def conv_rect(im_rect, fit_rect, fit_stride):
    w, h = (np.array(im_rect) - np.array(fit_rect)) / np.array(fit_stride) + 1
    assert w == int(w) and h == int(h)
    return int(w), int(h)

def iconv_rect(col_rect, fit_rect, fit_stride):
    w, h = (np.array(col_rect) - 1) * np.array(fit_stride) + np.array(fit_rect)
    assert w == int(w) and h == int(h)
    return int(w), int(h)


def im2col(im, fit_rect, fit_stride):
    from skimage.util import view_as_windows
    im_chan = im.shape[0]
    row_num = im_chan * fit_rect[0] * fit_rect[1]
    return view_as_windows(
                im, 
                (im_chan, fit_rect[0], fit_rect[1]), 
                (1, fit_stride[0], fit_stride[1])).reshape((-1,row_num)).T

def dim2col(dcol, col, im, fit_rect, fit_stride):
    from skimage.util import view_as_windows
    im_chan = im.shape[0]
    dim = np.zeros(im.shape)
    v = view_as_windows(
                dim, 
                (im_chan, fit_rect[0], fit_rect[1]), 
                (1, fit_stride[0], fit_stride[1]))
    v += dcol.T.reshape(v.shape)
    return dim * (1 / (fit_rect[0] * fit_rect[1]))


def max_pool(x):
    return np.max(x, axis=0)

def dmax_pool(dy, y, x):
    dx = np.zeros(x.shape)
    dx[np.argmax(x, axis=0), :] = dy
    return dx

