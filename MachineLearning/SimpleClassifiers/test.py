#vim:fileencoding=utf-8


import sys
sys.path.append('.')
sys.path.append('../data')

import utils


def evaluate_knearestneighbor(train_data, valid_data, test_data):
    from knearestneighbor_classifier import knearestneighbor_classifier
    train_data = train_data[:2000]
    test_data = test_data[:300]

    for k in range(1, 7, 2):
        classifier = knearestneighbor_classifier(k, fdist=utils.manhattan_dist)
        classifier.train(train_data, valid_data)
        acc = classifier.evaluate(test_data)
        print('%d nearest neighbour: %.2f%%' % (k, acc * 100))


def evaluate_linear(train_data, valid_data, test_data):
    from linear_classifier import linear_classifier2 as linear_classifier

    train_data = train_data[:2000]
    valid_data = valid_data[:300]
    test_data = test_data

    batch_size = 64

    classifier = linear_classifier(fact=utils.identity)

    classifier.train(
        train_data, valid_data,
        verbose=True, epoch=20, batch_size=batch_size, 
        weight_decay=0.0000,
        floss=utils.softmax_cross_entropy_loss, 
        optimizer=utils.sgd_optimizer,
        lr_scheduler=utils.step_learning_rate_scheduler(200.10, 5, 0.70),
        weights_initializer=utils.gaussian_weights_intializer(0, 0.5))

    acc = classifier.evaluate(test_data, batch_size=batch_size)

    print('linear: %.2f%%' % (acc * 100))


def evaluate_seq_nn(train_data, valid_data, test_data):
    from seq_nn_classifier import seq_nn_classifier as seq_nn_classifier 

    train_data = train_data[:2000]
    valid_data = valid_data[:300]
    test_data = test_data

    batch_size = 64
    din = train_data[0][0].shape[0]
    dout = train_data[0][1].shape[0]

    classifier = seq_nn_classifier(
        sizes=[din, 30, dout],
        facts=[utils.reLU, utils.identity])

    classifier.train(
        train_data, valid_data,
        verbose=True, epoch=20, batch_size=batch_size, 
        weight_decay=0.00000,
        floss=utils.softmax_cross_entropy_loss, 
        optimizer=utils.sgd_optimizer,
        lr_scheduler=utils.step_learning_rate_scheduler(50, 1, 0.915),
        weights_initializer=utils.gaussian_weights_intializer(0, 0.5))

    acc = classifier.evaluate(test_data, batch_size=batch_size)

    print('seq neuralnetwork: %.2f%%' % (acc * 100))


def test_mnist():
    from mnist_loader import load_data_wrapper as load_mnist

    train_data, valid_data, test_data = load_mnist()

    # evaluate_knearestneighbor(train_data, valid_data, test_data)
    # evaluate_linear(train_data, valid_data, test_data)
    evaluate_seq_nn(train_data, valid_data, test_data)


if __name__ == '__main__':
    test_mnist()
