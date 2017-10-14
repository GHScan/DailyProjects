#vim:fileencoding=utf-8


import sys
sys.path.append('../DataLoader')


def evaluate_mxnet_nn(train_data, valid_data, test_data):
    import mxnet
    from mxnet import gluon
    from mxnet import lr_scheduler
    from mxnet.gluon import nn
    import mxnet_utils as utils

    odim = train_data[0][1].shape[0]

    ctx = mxnet.gpu()
    batch_size = 1024

    train_loader = utils.as_dataloader(train_data, batch_size)
    valid_loader = utils.as_dataloader(valid_data, batch_size)
    test_loader = utils.as_dataloader(test_data, batch_size)

    net = nn.Sequential()
    with net.name_scope():
        net.add(nn.Dense(30, activation='relu'))
        net.add(nn.Dense(odim))
    net.initialize(init='Xavier', ctx=ctx)
    
    floss = gluon.loss.SoftmaxCrossEntropyLoss()

    lr_sch = lr_scheduler.FactorScheduler(1024, 0.99)

    trainer = gluon.Trainer(
                    net.collect_params(), 
                    'sgd', 
                    {'learning_rate':0.5, 'momentum':0.9, 'lr_scheduler':lr_sch, 'wd':0.0003})
    
    utils.train(ctx, net, floss, trainer, train_loader, valid_loader, 
                epochs=50, valid_interval=5)

    print('mxnet_nn: %.2f%%' % utils.evaluate(ctx, net, test_loader))


def evaluate_mxnet_LeNet(train_data, valid_data, test_data, rect):
    import mxnet
    from mxnet import gluon
    from mxnet.gluon import nn
    from mxnet import lr_scheduler
    import mxnet_utils as utils

    odim = train_data[0][1].shape[0]

    ctx = mxnet.gpu()
    batch_size = 1024

    train_loader = utils.as_dataloader(train_data, batch_size, rect)
    valid_loader = utils.as_dataloader(valid_data, batch_size, rect)
    test_loader = utils.as_dataloader(test_data, batch_size, rect)

    net = nn.Sequential()
    with net.name_scope():
        net.add(
            nn.Conv2D(channels=20, kernel_size=5, activation='relu'),
            nn.MaxPool2D(pool_size=2, strides=2),
            nn.Conv2D(channels=50, kernel_size=3, activation='relu'),
            nn.MaxPool2D(pool_size=2, strides=2),
            nn.Flatten(),
            nn.Dense(128, activation="relu"),
            nn.Dense(odim)
                )
    net.initialize(init='Xavier', ctx=ctx)
    
    floss = gluon.loss.SoftmaxCrossEntropyLoss()

    lr_sch = lr_scheduler.FactorScheduler(1024, 0.95)

    trainer = gluon.Trainer(
                    net.collect_params(), 
                    'sgd', 
                    {'learning_rate':0.05, 'momentum':0.9, 'lr_scheduler':lr_sch, 'wd':0.0030})
    
    utils.train(ctx, net, floss, trainer, train_loader, valid_loader, 
                epochs=50, valid_interval=5)

    print('mxnet_cnn: %.2f%%' % utils.evaluate(ctx, net, test_loader))


def evaluate_mxnet_LeNet2(train_data, valid_data, test_data, rect):
    import mxnet
    from mxnet import gluon
    from mxnet.gluon import nn
    from mxnet import lr_scheduler
    import mxnet_utils as utils

    odim = train_data[0][1].shape[0]

    ctx = mxnet.gpu()
    batch_size = 1024

    train_loader = utils.as_dataloader(train_data, batch_size, rect)
    valid_loader = utils.as_dataloader(valid_data, batch_size, rect)
    test_loader = utils.as_dataloader(test_data, batch_size, rect)

    net = nn.Sequential()
    with net.name_scope():
        net.add(
            nn.Conv2D(channels=48, kernel_size=5, activation='relu'),
            nn.MaxPool2D(pool_size=2, strides=2),
            nn.Conv2D(channels=128, kernel_size=3, activation='relu'),
            nn.MaxPool2D(pool_size=2, strides=2),
            nn.Conv2D(channels=512, kernel_size=1, activation='relu'),
            nn.Flatten(),
            nn.Dense(1000, activation="relu"),
            nn.Dropout(0.5),
            nn.Dense(1000, activation="relu"),
            nn.Dense(odim)
                )
    net.initialize(init='Xavier', ctx=ctx)
    
    floss = gluon.loss.SoftmaxCrossEntropyLoss()

    lr_sch = lr_scheduler.FactorScheduler(1024, 0.95)

    trainer = gluon.Trainer(
                    net.collect_params(), 
                    'sgd', 
                    {'learning_rate':0.05, 'momentum':0.9, 'lr_scheduler':lr_sch, 'wd':0.0030})
    
    utils.train(ctx, net, floss, trainer, train_loader, valid_loader, 
                epochs=50, valid_interval=5)

    print('mxnet_cnn: %.2f%%' % utils.evaluate(ctx, net, test_loader))


def evaluate_caffe_nn(train_data, valid_data, test_data):
    import caffe
    from caffe import layers as L, params as P
    import caffe_utils as utils

    def gen_net(net_path, data_shape, label_shape):
        net = caffe.NetSpec()
        
        net.data = L.Input(shape=dict(dim=list(data_shape)))
        net.label = L.Input(shape=dict(dim=list(label_shape)))
        
        net.fc0 = L.InnerProduct(net.data, num_output=30, weight_filler=dict(type='xavier'))
        net.relu0 = L.ReLU(net.fc0, in_place=True)
        net.output = L.InnerProduct(net.relu0, num_output=10, weight_filler=dict(type='xavier'))

        net.loss = L.SoftmaxWithLoss(net.output, net.label)
        net.accuracy = L.Accuracy(net.output, net.label)
        
        with open(net_path, 'w') as f:
            f.write(str(net.to_proto()))

    def gen_solver(solver_path, net_path):
        from caffe.proto import caffe_pb2

        params = caffe_pb2.SolverParameter()

        params.train_net = net_path
        
        params.type = 'SGD'
        params.momentum = 0.9
        params.base_lr = 0.5

        params.lr_policy = 'step'
        params.gamma = 0.999
        params.stepsize = 1

        params.weight_decay = 0.0003

        with open(solver_path, 'w') as f:
            f.write(str(params))

    batch_size = 1024
    data_shape = (batch_size, train_data[0][0].size)
    label_shape = (batch_size, 1)
    train_blobs = utils.as_dl_blobs(train_data, batch_size, data_shape, label_shape)
    valid_blobs = utils.as_dl_blobs(valid_data, batch_size, data_shape, label_shape)
    test_blobs = utils.as_dl_blobs(test_data, batch_size, data_shape, label_shape)

    net_path = 'temp/net.prototxt'
    solver_path = 'temp/solver.txt'
    gen_net(net_path, data_shape, label_shape)
    gen_solver(solver_path, net_path)

    caffe.set_device(0)
    caffe.set_mode_gpu()

    solver = caffe.SGDSolver(solver_path)

    utils.train(solver, train_blobs, valid_blobs, 50, 5)
    print('caffe nn: %.2f%%' % (utils.evaluate(solver, test_blobs)))
     

def evaluate_caffe_LeNet(train_data, valid_data, test_data, rect):
    import caffe
    from caffe import layers as L, params as P
    import caffe_utils as utils

    def gen_net(net_path, data_shape, label_shape):
        net = caffe.NetSpec()
        
        net.data = L.Input(shape=dict(dim=list(data_shape)))
        net.label = L.Input(shape=dict(dim=list(label_shape)))
        
        net.conv1 = L.Convolution(net.data, kernel_size=5, num_output=20, weight_filler=dict(type='xavier'))
        net.pool1 = L.Pooling(net.conv1, kernel_size=2, stride=2, pool=P.Pooling.MAX)
        net.conv2 = L.Convolution(net.pool1, kernel_size=3, num_output=50, weight_filler=dict(type='xavier'))
        net.pool2 = L.Pooling(net.conv2, kernel_size=2, stride=2, pool=P.Pooling.MAX)
        net.fc1 = L.InnerProduct(net.pool2, num_output=128, weight_filler=dict(type='xavier'))
        net.relu1 = L.ReLU(net.fc1, in_place=True)
        net.output = L.InnerProduct(net.relu1, num_output=10, weight_filler=dict(type='xavier'))

        net.loss = L.SoftmaxWithLoss(net.output, net.label)
        net.accuracy = L.Accuracy(net.output, net.label)
        
        with open(net_path, 'w') as f:
            f.write(str(net.to_proto()))

    def gen_solver(solver_path, net_path):
        from caffe.proto import caffe_pb2

        params = caffe_pb2.SolverParameter()

        params.train_net = net_path
        
        params.type = 'SGD'
        params.momentum = 0.9
        params.base_lr = 0.05

        params.lr_policy = 'step'
        params.gamma = 0.95
        params.stepsize = 1

        params.weight_decay = 0.003

        with open(solver_path, 'w') as f:
            f.write(str(params))

    batch_size = 1024
    data_shape = (batch_size, train_data[0][0].size / (rect[0] * rect[1])) + rect
    label_shape = (batch_size, 1)
    train_blobs = utils.as_dl_blobs(train_data, batch_size, data_shape, label_shape)
    valid_blobs = utils.as_dl_blobs(valid_data, batch_size, data_shape, label_shape)
    test_blobs = utils.as_dl_blobs(test_data, batch_size, data_shape, label_shape)

    net_path = 'temp/net.prototxt'
    solver_path = 'temp/solver.txt'
    gen_net(net_path, data_shape, label_shape)
    gen_solver(solver_path, net_path)

    caffe.set_device(0)
    caffe.set_mode_gpu()

    solver = caffe.SGDSolver(solver_path)

    utils.train(solver, train_blobs, valid_blobs, 50, 5)
    print('caffe nn: %.2f%%' % (utils.evaluate(solver, test_blobs)))


def evaluate_caffe_LeNet2(train_data, valid_data, test_data, rect):
    import caffe
    from caffe import layers as L, params as P
    import caffe_utils as utils

    def gen_net(net_path, data_shape, label_shape):
        net = caffe.NetSpec()
        
        net.data = L.Input(shape=dict(dim=list(data_shape)))
        net.label = L.Input(shape=dict(dim=list(label_shape)))
        
        net.conv1 = L.Convolution(net.data, kernel_size=5, num_output=48, weight_filler=dict(type='xavier'))
        net.pool1 = L.Pooling(net.conv1, kernel_size=2, stride=2, pool=P.Pooling.MAX)
        net.conv2 = L.Convolution(net.pool1, kernel_size=3, num_output=128, weight_filler=dict(type='xavier'))
        net.pool2 = L.Pooling(net.conv2, kernel_size=2, stride=2, pool=P.Pooling.MAX)
        net.conv3 = L.Convolution(net.pool2, kernel_size=1, num_output=512, weight_filler=dict(type='xavier'))
        net.fc1 = L.InnerProduct(net.conv3, num_output=1000, weight_filler=dict(type='xavier'))
        net.relu1 = L.ReLU(net.fc1, in_place=True)
        net.dropout1 = L.Dropout(net.relu1, dropout_ratio=0.5)
        net.fc2 = L.InnerProduct(net.dropout1, num_output=1000, weight_filler=dict(type='xavier'))
        net.relu2 = L.ReLU(net.fc2, in_place=True)
        net.output = L.InnerProduct(net.relu2, num_output=10, weight_filler=dict(type='xavier'))

        net.loss = L.SoftmaxWithLoss(net.output, net.label)
        net.accuracy = L.Accuracy(net.output, net.label)
        
        with open(net_path, 'w') as f:
            f.write(str(net.to_proto()))

    def gen_solver(solver_path, net_path):
        from caffe.proto import caffe_pb2

        params = caffe_pb2.SolverParameter()

        params.train_net = net_path
        
        params.type = 'SGD'
        params.momentum = 0.9
        params.base_lr = 0.05

        params.lr_policy = 'step'
        params.gamma = 0.95
        params.stepsize = 1

        params.weight_decay = 0.003

        with open(solver_path, 'w') as f:
            f.write(str(params))

    batch_size = 1024
    data_shape = (batch_size, train_data[0][0].size / (rect[0] * rect[1])) + rect
    label_shape = (batch_size, 1)
    train_blobs = utils.as_dl_blobs(train_data, batch_size, data_shape, label_shape)
    valid_blobs = utils.as_dl_blobs(valid_data, batch_size, data_shape, label_shape)
    test_blobs = utils.as_dl_blobs(test_data, batch_size, data_shape, label_shape)

    net_path = 'temp/net.prototxt'
    solver_path = 'temp/solver.txt'
    gen_net(net_path, data_shape, label_shape)
    gen_solver(solver_path, net_path)

    caffe.set_device(0)
    caffe.set_mode_gpu()

    solver = caffe.SGDSolver(solver_path)

    utils.train(solver, train_blobs, valid_blobs, 50, 5)
    print('caffe nn: %.2f%%' % (utils.evaluate(solver, test_blobs)))


def test_mnist():
    from mnist_loader_py2 import load_data_wrapper as load_mnist

    train_data, valid_data, test_data = load_mnist()

    #evaluate_mxnet_nn(train_data, valid_data, test_data)
    #evaluate_mxnet_LeNet(train_data, valid_data, test_data, (1,28,28))
    #evaluate_mxnet_LeNet2(train_data, valid_data, test_data, (1,28,28))
    #evaluate_caffe_nn(train_data, valid_data, test_data)
    #evaluate_caffe_LeNet(train_data, valid_data, test_data, (28,28))
    evaluate_caffe_LeNet2(train_data, valid_data, test_data, (28,28))


def test_cifar10():
    from cifar10_loader_py2 import load_cifar10

    train_data, valid_data, test_data = load_cifar10()

    #evaluate_mxnet_nn(train_data, valid_data, test_data)
    #evaluate_mxnet_LeNet(train_data, valid_data, test_data, (3,32,32))
    #evaluate_mxnet_LeNet2(train_data, valid_data, test_data, (3,32,32))
    #evaluate_caffe_nn(train_data, valid_data, test_data)
    #evaluate_caffe_LeNet(train_data, valid_data, test_data, (32,32))
    evaluate_caffe_LeNet2(train_data, valid_data, test_data, (32,32))


if __name__ == '__main__':
    #test_mnist()
    test_cifar10()
