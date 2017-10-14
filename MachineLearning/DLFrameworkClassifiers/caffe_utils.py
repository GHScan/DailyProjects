#vim:fileencoding=utf-8


def break_into_batches(X, batch_size, shuffle=False):
    idx = list(range(len(X)))
    if shuffle:
        random.shuffle(idx)

    l = []
    for off in range(0, len(idx), batch_size):
        del l[:]
        for i in idx[off:off + batch_size]:
            l.append(X[i])
        yield l


def as_dl_blobs(data, batch_size, data_shape, label_shape):
    import numpy as np

    data_blobs, label_blobs = [], []

    for batch in break_into_batches(data, batch_size):
        if len(batch) < data_shape[0]:
            break
        data_blobs.append(np.array([x for x, _ in batch]).reshape(data_shape))
        label_blobs.append(np.array([np.argmax(y) for _, y in batch]).reshape(label_shape))

    return zip(data_blobs, label_blobs)


def train_one_epoch(solver, train_blobs):
    n = len(train_blobs)
    total_acc, total_loss = 0, 0

    for data, label in train_blobs:
        solver.net.blobs['data'].data[...] = data
        solver.net.blobs['label'].data[...] = label

        solver.step(1)

        total_acc += solver.net.blobs['accuracy'].data[...]
        total_loss += solver.net.blobs['loss'].data[...]

    return total_acc * 100 / n, total_loss / n


def train(solver, train_blobs, valid_blobs, epochs, valid_interval):
    import time

    for e in range(epochs):
        tic = time.time()

        train_acc, train_loss = train_one_epoch(solver, train_blobs)
        
        if (e + 1) % valid_interval == 0:
            valid_acc = evaluate(solver, valid_blobs)

            print('epoch: %d, acc=%.2f%%, loss=%.5f, valid_acc=%.2f%%, elapse=%f' 
                    % (e + 1, train_acc, train_loss, valid_acc, time.time() - tic))


def evaluate(solver, test_blobs):
    total_acc = 0

    for data, label in test_blobs:
        solver.net.blobs['data'].data[...] = data
        solver.net.blobs['label'].data[...] = label
        
        solver.net.forward()

        total_acc += solver.net.blobs['accuracy'].data[...]

    return total_acc * 100 / len(test_blobs)
