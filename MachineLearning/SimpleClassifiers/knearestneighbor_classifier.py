#vim:fileencoding=utf-8


import numpy as np
import utils


class knearestneighbor_classifier(object):
    def __init__(self, k, fdist):
        self.k = k
        self.fdist = fdist

    def train(self, train_data, valid_data):
        self.train_X = np.array([ x for x, _ in train_data ])
        self.train_Y = np.array([ y for _, y in train_data ])

    def evaluate(self, test_data):
        succ = 0
        for x, y in test_data:
            if (y == self.predicate(x)).all():
                succ = succ + 1
        return succ / len(test_data)

    def predicate(self, x):
        dis = self.fdist(self.train_X, x)
        nearest_idx = np.argsort(dis)[:self.k]
        nearest_y_vec = self.train_Y[nearest_idx, :]
        y_class = np.argmax(np.sum(nearest_y_vec, axis=0))
        return utils.int_to_vector(y_class, self.train_Y[0].shape[0])
