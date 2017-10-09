#vim:fileencoding=utf-8


import numpy as np
import utils


class seq_nn_classifier(object):
    def __init__(self, sizes, facts):
        self.sizes = sizes
        self.facts = facts
        self.fdacts = [utils.lookup_dfunc(fact) for fact in facts]

    def train(self, train_data, valid_data, 
            verbose, epoch, batch_size, 
            weight_decay, floss, optimizer, lr_scheduler, weights_initializer):

        self.weights = [weights_initializer((din, dout)) for din, dout in zip(self.sizes[:-1], self.sizes[1:]) ]
        self.biases = [np.zeros((dout,)) for dout in self.sizes[1:] ]

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
            act = X
            actws = []
            actwbs = []
            acts = [act]
            for w, b, fact in zip(self.weights, self.biases, self.facts):
                actw = np.dot(act, w)
                actwb = actw + b
                act = fact(actwb)
                actws.append(actw)
                actwbs.append(actwb)
                acts.append(act)
            Y = act
            Loss = floss(Y, Y_expect)

            # backward
            dws = []
            dbs = []
            dact = fdloss(Loss, Y, Y_expect)
            for actwb, actw, prev_act, w, fdact in reversed(
                    list(zip(actwbs, actws, acts, self.weights, self.fdacts))):
                dactwb = fdact(dact, act, actwb)
                dactw = dactwb
                db = np.sum(dactwb, axis=0)
                dw = utils.ddot1(dactw, actw, prev_act, w)
                dact = utils.ddot0(dactw, actw, prev_act, w)
                act = prev_act
                dws.append(dw)
                dbs.append(db)
            dws.reverse()
            dbs.reverse()

            # metrics
            total_loss += np.sum(Loss)
            total_succ += np.sum(Y_expect[range(Y_expect.shape[0]), np.argmax(Y, axis=-1)])

            # optimize
            lr = lr_scheduler(epoch)
            optimizer(self.weights + self.biases, dws + dbs, n, batch_size, lr, weight_decay)

        return total_succ / n, total_loss / n 

    def evaluate(self, test_data, batch_size):
        total_succ = 0
        for batch in utils.break_into_batches(test_data, batch_size):
            X = np.array([x for x, _ in batch])
            Y_expect = np.array([y for _, y in batch])

            act = X
            for w, b, fact in zip(self.weights, self.biases, self.facts):
                act = fact(np.dot(act, w) + b)
            Y = act

            total_succ += np.sum(Y_expect[range(Y_expect.shape[0]), np.argmax(Y, axis=-1)])

        return total_succ / len(test_data)

    def predicate(self, x):
        act = x
        for w, b, fact in zip(self.weights, self.biases, self.facts):
            act = fact(self.np.dot(act, w) + b)
        return act == np.max(act)
