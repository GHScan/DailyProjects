#vim:fileencoding=utf-8


from mxnet import gluon
from mxnet import autograd
from mxnet import nd
from mxnet.gluon import nn


def as_dataloader(data, batch_size=64, rect=(-1,), shuffle=True):
    import numpy as np

    dataset = gluon.data.ArrayDataset(
                [x.reshape(rect) for x, _ in data], 
                [np.array(np.argmax(y), dtype=np.float32) for _, y in data])

    return gluon.data.DataLoader(
                dataset=dataset, 
                batch_size=batch_size, 
                shuffle=shuffle)


def train_one_epoch(ctx, net, floss, trainer, data_loader):
    n, total_loss, total_succ = 0, 0, 0

    for X, y_expect in data_loader:
        X, y_expect = X.as_in_context(ctx), y_expect.as_in_context(ctx)

        with autograd.record():
            y = net(X)
            loss = floss(y, y_expect)

        loss.backward()
        trainer.step(X.shape[0])

        total_loss += loss.sum().asscalar()
        total_succ += nd.sum(y.argmax(axis=1) == y_expect).asscalar()
        n += X.shape[0]

    return 100 * total_succ / n, total_loss / n

def evaluate(ctx, net, data_loader):
    n, total_succ = 0, 0

    for X, y_expect in data_loader:
        X, y_expect = X.as_in_context(ctx), y_expect.as_in_context(ctx)

        y = net(X)
        total_succ += nd.sum(y.argmax(axis=1) == y_expect).asscalar()
        n += X.shape[0]

    return 100 * total_succ / n


def train(ctx, net, floss, trainer, train_loader, valid_loader, epochs, valid_interval):
    import time

    for e in range(epochs):
        tic = time.time()

        train_acc, train_loss = train_one_epoch(ctx, net, floss, trainer, train_loader)

        if (e + 1) % valid_interval == 0:
            valid_acc = evaluate(ctx, net, valid_loader)

            print('epoch: %d, acc=%.2f%%, loss=%.5f, valid_acc=%.2f%%, elapse=%.3f' %
                    (e + 1, train_acc, train_loss, valid_acc, time.time() - tic))
