#vim:fileencoding=utf-8


import pickle
import numpy as np


def int_to_vector(i, max_i):
    v = np.zeros(max_i)
    v[i] = 1
    return v


def load_data(path):
    with open(path, 'rb') as fo:
        package = pickle.load(fo, encoding='bytes')

        data = (row.reshape((3,-1)).astype('float32') / 255 for row in package[b'data'])
        labels = (int_to_vector(i, 10) for i in package[b'labels'])

        return list(zip(data, labels))


def load_cifar10():
    data = load_data('../data/cifar-10-batches-py/data_batch_1')

    train_data = data[:8000]
    valid_data = data[8000:10000]
    test_data = load_data('../data/cifar-10-batches-py/test_batch')[:8000]

    return train_data, valid_data, test_data
