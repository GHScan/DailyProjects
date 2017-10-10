"""
mnist_loader
~~~~~~~~~~~~

A library to load the MNIST image data.  For details of the data
structures that are returned, see the doc strings for ``load_data``
and ``load_data_wrapper``.  In practice, ``load_data_wrapper`` is the
function usually called by our neural network code.
"""

#### Libraries
# Standard library
import _pickle
import gzip

# Third-party libraries
import numpy as np

def load_data():
    """Return the MNIST data as a tuple containing the training data,
    the validation data, and the test data.

    The ``training_data`` is returned as a tuple with two entries.
    The first entry contains the actual training images.  This is a
    numpy ndarray with 50,000 entries.  Each entry is, in turn, a
    numpy ndarray with 784 values, representing the 28 * 28 = 784
    pixels in a single MNIST image.

    The second entry in the ``training_data`` tuple is a numpy ndarray
    containing 50,000 entries.  Those entries are just the digit
    values (0...9) for the corresponding images contained in the first
    entry of the tuple.

    The ``validation_data`` and ``test_data`` are similar, except
    each contains only 10,000 images.

    This is a nice data format, but for use in neural networks it's
    helpful to modify the format of the ``training_data`` a little.
    That's done in the wrapper function ``load_data_wrapper()``, see
    below.
    """
    f = gzip.open('../data/mnist.pkl.gz', 'rb')
    training_data, validation_data, test_data = _pickle.load(f, encoding='bytes')
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
