#vim:fileencoding=utf-8


import pickle
import numpy as np


def int_to_vector(i, max_i):
    v = np.zeros(max_i)
    v[i] = 1
    return v


def load_data(path):
    with open(path, 'rb') as fo:
        package = pickle.load(fo)

        data = (row.reshape((3,-1)).astype('float32') / 255 for row in package[b'data'])
        labels = (int_to_vector(i, 10) for i in package[b'labels'])

        return list(zip(data, labels))


def load_cifar10():
    train_data = load_data('../data/cifar-10-batches-py/data_batch_1') +\
                    load_data('../data/cifar-10-batches-py/data_batch_2') +\
                    load_data('../data/cifar-10-batches-py/data_batch_3') +\
                    load_data('../data/cifar-10-batches-py/data_batch_4')
    valid_data = load_data('../data/cifar-10-batches-py/data_batch_5')
    test_data = load_data('../data/cifar-10-batches-py/test_batch')

    return train_data, valid_data, test_data
