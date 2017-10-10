#vim:fileencoding=utf-8

import numpy as np
import utils


class conv_layer(object):
    def __init__(self, fit_chan, fit_rect, fit_stride, fact):
        self.fit_chan = fit_chan
        self.fit_rect = fit_rect
        self.fit_stride = fit_stride
        self.fact = fact
        self.fdact = utils.lookup_dfunc(fact)

    def initialize(self, im_chan, im_rect, weights_initializer):
        self.im_rect = im_rect
        self.col_rect = utils.conv_rect(self.im_rect, self.fit_rect, self.fit_stride)
        self.weights = weights_initializer((self.fit_chan, im_chan * self.fit_rect[0] * self.fit_rect[1]))
        self.biases = np.zeros((self.fit_chan, 1))

        self.params = [self.weights, self.biases]

    def forward(self, x):
        self._im = x.reshape((x.shape[0],self.im_rect[0],self.im_rect[1]))
        self._t0 = utils.im2col(self._im, self.fit_rect, self.fit_stride)
        self._t1 = np.dot(self.weights, self._t0)
        self._t2 = self._t1 + self.biases
        return self.fact(self._t2)

    def backward(self, dy, y, x):
        dt2 = self.fdact(dy, y, self._t2)
        self.biases_grad = np.sum(dt2, axis=-1).reshape((-1,1))
        dt1 = dt2
        self.weights_grad = utils.ddot0(dt1, self._t1, self.weights, self._t0)
        dt0 = utils.ddot1(dt1, self._t1, self.weights, self._t0)
        dim = utils.dim2col(dt0, self._t0, self._im, self.fit_rect, self.fit_stride)
        dx = dim.reshape((dim.shape[0],-1))

        scale = 1 / (self.col_rect[0] * self.col_rect[1])
        self.params_grad = [self.weights_grad * scale, self.biases_grad * scale]

        return dx


class max_pool_layer(object):
    def __init__(self, pool_rect, pool_stride):
        self.pool_rect = pool_rect
        self.pool_stride = pool_stride

    def initialize(self, im_chan, im_rect, weights_initializer):
        self.fit_chan = im_chan
        self.im_rect = im_rect
        self.col_rect = utils.conv_rect(im_rect, self.pool_rect, self.pool_stride)

        self.params = []

    def forward(self, x):
        self._ims = []
        self._cols = []
        new_rows = []
        for row in x:
            im = row.reshape((1,self.im_rect[0],self.im_rect[1]))
            col = utils.im2col(im, self.pool_rect, self.pool_stride)
            new_row = utils.max_pool(col)
            new_rows.append(new_row)
            self._ims.append(im)
            self._cols.append(col)
        return np.array(new_rows)

    def backward(self, dy, y, x):
        drows = []
        for dnew_row, new_row, row, col, im in zip(dy, y, x, self._cols, self._ims):
            dcol = utils.dmax_pool(dnew_row, new_row, col)
            dim = utils.dim2col(dcol, col, im, self.pool_rect, self.pool_stride)
            drow = dim.reshape(-1)
            drows.append(drow)

        self.params_grad = []

        return np.array(drows)


class fc_layer(object):
    def __init__(self, output_size, fact):
        self.output_size = output_size
        self.fact = fact
        self.fdact = utils.lookup_dfunc(fact)

    def initialize(self, input_size, weights_initializer):
        self.input_size = input_size
        self.weights = weights_initializer((self.input_size, self.output_size))
        self.bias = np.zeros((1,self.output_size))

        self.params = [ self.weights, self.bias ]

    def forward(self, x):
        self._t0 = np.dot(x, self.weights)
        self._t1 = self._t0 + self.bias
        return self.fact(self._t1)

    def backward(self, dy, y, x):
        dt1 = self.fdact(dy, y, self._t1)
        self.bias_grad = dt1
        dt0 = dt1
        dx = utils.ddot0(dt0, self._t0, x, self.weights)
        self.weights_grad = utils.ddot1(
                dt0.reshape((1,-1)),
                self._t0, 
                x.reshape((1,-1)),
                self.weights)

        self.params_grad = [ self.weights_grad, self.bias_grad ]

        return dx


class seq_cnn_classifier(object):
    def __init__(self, conv_layers, fc_layers):
        self.conv_layers = conv_layers
        self.fc_layers = fc_layers

    def all_layers(self):
        return self.conv_layers + self.fc_layers

    def train(self, train_data, valid_data, sample_rect,
            verbose, epoch, batch_size, 
            weight_decay, floss, optimizer, lr_scheduler, weights_initializer):

        im_chan = train_data[0][0].shape[0]
        im_rect = sample_rect
        for conv_layer in self.conv_layers:
            conv_layer.initialize(im_chan, im_rect, weights_initializer)
            im_chan = conv_layer.fit_chan
            im_rect = conv_layer.col_rect

        input_size = im_chan * im_rect[0] * im_rect[1]
        for fc_layer in self.fc_layers:
            fc_layer.initialize(input_size, weights_initializer)
            input_size = fc_layer.output_size

        for e in range(epoch):
            acc, loss = self.run_one_epoch(e, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler)
            if verbose:
                valid_acc = self.evaluate(valid_data, batch_size)
                print('epoch=%d, acc=%.2f%%, loss=%f, valid_acc=%.2f%%' % 
                        (e + 1, acc * 100, loss, valid_acc * 100))

    def run_one_epoch(self, epoch, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler):

        fdloss = utils.lookup_dfunc(floss)
        params = [ param for layer in self.all_layers() for param in layer.params ]

        n = len(train_data)
        total_loss = 0
        total_acc = 0

        for batch in utils.break_into_batches(train_data, batch_size, shuffle=True):
            batch_grads = []

            for x, y_expect in batch:

                # forward
                y = self.forward(x)
                loss = floss(y, y_expect)

                # backward
                dy = fdloss(loss, y, y_expect)
                self.backward(dy, y, x)

                # update grad
                grads = [ grad for layer in self.all_layers() for grad in layer.params_grad ]
                if not batch_grads:
                    batch_grads = grads
                else:
                    for batch_grad, grad in zip(batch_grads, grads):
                        batch_grad += grad

                # metrics
                total_loss += loss
                total_acc += y_expect[np.argmax(y)] == 1

            # optimize
            lr = lr_scheduler(epoch)
            optimizer(params, batch_grads, n, batch_size, lr, weight_decay)

        return total_acc / n, total_loss / n 

    def forward(self, x):
        self._xs = []

        for conv_layer in self.conv_layers:
            self._xs.append(x)
            x = conv_layer.forward(x)

        x = x.reshape(-1)

        for fc_layer in self.fc_layers:
            self._xs.append(x)
            x = fc_layer.forward(x)

        return x

    def backward(self, dy, y, _):
        for fc_layer, x in zip(reversed(self.fc_layers), reversed(self._xs)):
            dx = fc_layer.backward(dy, y, x)
            dy, y = dx, x

        if not self.conv_layers:
            return dx

        last_chan = self.conv_layers[-1].fit_chan
        dy, y = dy.reshape((last_chan,-1)), y.reshape((last_chan,-1))

        for conv_layer, x in zip(reversed(self.conv_layers), reversed(self._xs[:len(self.conv_layers)])):
            dx = conv_layer.backward(dy, y, x)
            dy, y = dx, x

        return dx

    def evaluate(self, test_data, batch_size):
        succ = 0
        for x, y in test_data:
            if (y == self.predicate(x)).all():
                succ = succ + 1
        return succ / len(test_data)

    def predicate(self, x):
        act = self.forward(x)
        return act == np.max(act)
