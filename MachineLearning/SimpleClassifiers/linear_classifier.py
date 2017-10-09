#vim:fileencoding=utf-8


import numpy as np
import utils


class linear_classifier(object):
    def __init__(self, fact):
        self.fact = fact
        self.fdact = utils.lookup_dfunc(fact)

    def train(self, train_data, valid_data, 
            verbose, epoch, batch_size, 
            weight_decay, floss, optimizer, lr_scheduler, weights_initializer):

        din, dout = train_data[0][0].shape[0], train_data[0][1].shape[0]

        self.w = weights_initializer((dout, din))
        self.b = np.zeros((dout,))

        for e in range(epoch):
            acc, loss = self.run_one_epoch(e, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler)
            if verbose:
                valid_acc = self.evaluate(valid_data, batch_size)
                print('epoch=%d, acc=%.2f%%, loss=%f, valid_acc=%.2f%%' % 
                        (e + 1, acc * 100, loss, valid_acc * 100))

    def run_one_epoch(self, epoch, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler):

        fdloss = utils.lookup_dfunc(floss)

        n = len(train_data)
        total_loss = 0
        total_acc = 0

        for batch in utils.break_into_batches(train_data, batch_size, shuffle=True):
            grad_w = np.zeros(self.w.shape)
            grad_b = np.zeros(self.b.shape)

            for x, y_expect in batch:

                # forward
                wx = np.dot(self.w, x)
                wxpb = wx + self.b
                y = self.fact(wxpb)
                loss = floss(y, y_expect)

                # backward
                dy = fdloss(loss, y, y_expect)
                dwxpb = self.fdact(dy, y, wxpb)
                dwx = dwxpb
                db = dwxpb
                dw = utils.ddot0(dwx.reshape((-1,1)), wx, self.w, x.reshape((-1,1)))

                # update grad
                grad_w += dw
                grad_b += db

                # metrics
                total_loss += loss
                total_acc += y_expect[np.argmax(y)] == 1

            # optimize
            lr = lr_scheduler(epoch)
            optimizer([self.w, self.b], [grad_w, grad_b], n, batch_size, lr, weight_decay)

        return total_acc / n, total_loss / n 

    def evaluate(self, test_data, batch_size):
        succ = 0
        for x, y in test_data:
            if (y == self.predicate(x)).all():
                succ = succ + 1
        return succ / len(test_data)

    def predicate(self, x):
        act = self.fact(np.dot(self.w, x) + self.b)
        return act == np.max(act)


class linear_classifier2(object):
    def __init__(self, fact):
        self.fact = fact
        self.fdact = utils.lookup_dfunc(fact)

    def train(self, train_data, valid_data, 
            verbose, epoch, batch_size, 
            weight_decay, floss, optimizer, lr_scheduler, weights_initializer):

        din, dout = train_data[0][0].shape[0], train_data[0][1].shape[0]

        self.w = weights_initializer((din, dout))
        self.b = np.zeros((dout,))

        for e in range(epoch):
            acc, loss = self.run_one_epoch(e, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler)
            if verbose:
                valid_acc = self.evaluate(valid_data, batch_size)
                print('epoch=%d, acc=%.2f%%, loss=%f, valid_acc=%.2f%%' % 
                        (e + 1, acc * 100, loss, valid_acc * 100))

    def run_one_epoch(self, epoch, train_data, batch_size, weight_decay, floss, optimizer, lr_scheduler):

        fdloss = utils.lookup_dfunc(floss)

        n = len(train_data)
        total_loss = 0
        total_succ = 0

        for batch in utils.break_into_batches(train_data, batch_size, shuffle=True):

            X = np.array([x for x, _ in batch])
            Y_expect = np.array([y for _, y in batch])

            # forward
            Xw = np.dot(X, self.w)
            Xwpb = Xw + self.b
            Y = self.fact(Xwpb)
            Loss = floss(Y, Y_expect)

            # backward
            dY = fdloss(Loss, Y, Y_expect)
            dXwpb = self.fdact(dY, Y, Xwpb)
            dXw = dXwpb
            db = np.sum(dXwpb, axis=0)
            dw = utils.ddot1(dXw, Xw, X, self.w)

            # metrics
            total_loss += np.sum(Loss)
            total_succ += np.sum(Y_expect[range(Y_expect.shape[0]), np.argmax(Y, axis=-1)])

            # optimize
            lr = lr_scheduler(epoch)
            optimizer([self.w, self.b], [dw, db], n, batch_size, lr, weight_decay)

        return total_succ / n, total_loss / n 

    def evaluate(self, test_data, batch_size):
        total_succ = 0
        for batch in utils.break_into_batches(test_data, batch_size):
            X = np.array([x for x, _ in batch])
            Y_expect = np.array([y for _, y in batch])

            Xw = np.dot(X, self.w)
            Xwpb = Xw + self.b
            Y = self.fact(Xwpb)

            total_succ += np.sum(Y_expect[range(Y_expect.shape[0]), np.argmax(Y, axis=-1)])

        return total_succ / len(test_data)

    def predicate(self, x):
        act = self.fact(np.dot(x, self.w) + self.b)
        return act == np.max(act)
