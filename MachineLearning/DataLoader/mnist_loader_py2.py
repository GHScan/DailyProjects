
import cPickle
import gzip

import numpy as np

def load_data():
    f = gzip.open('../data/mnist.pkl.gz', 'rb')
    training_data, validation_data, test_data = cPickle.load(f)
    f.close()
    return (training_data, validation_data, test_data)

def load_data_wrapper():
    (train_X, train_Y), (valid_X, valid_Y), (test_X, test_Y) = load_data()

    train_data = list(zip(train_X, [int_to_vector(y, 10) for y in train_Y]))

    valid_data = list(zip(valid_X, [int_to_vector(y, 10) for y in valid_Y]))

    test_data = list(zip(test_X, [int_to_vector(y, 10) for y in test_Y]))

    return train_data, valid_data, test_data

def int_to_vector(i, max_i):
    v = np.zeros(max_i)
    v[i] = 1
    return v
