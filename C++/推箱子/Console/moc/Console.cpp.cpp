/****************************************************************************
** Meta object code from reading C++ file 'Console.cpp'
**
** Created: Sun Nov 6 20:55:31 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Console.cpp' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_TXZSolutionGenerateThread[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      29,   27,   26,   26, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_TXZSolutionGenerateThread[] = {
    "TXZSolutionGenerateThread\0\0s\0"
    "solutionGenerated(QString)\0"
};

const QMetaObject TXZSolutionGenerateThread::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_TXZSolutionGenerateThread,
      qt_meta_data_TXZSolutionGenerateThread, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &TXZSolutionGenerateThread::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *TXZSolutionGenerateThread::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *TXZSolutionGenerateThread::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_TXZSolutionGenerateThread))
        return static_cast<void*>(const_cast< TXZSolutionGenerateThread*>(this));
    return QThread::qt_metacast(_clname);
}

int TXZSolutionGenerateThread::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: solutionGenerated((*reinterpret_cast< QString(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void TXZSolutionGenerateThread::solutionGenerated(QString _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_TXZSolutionGenerator[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x08,
      36,   34,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_TXZSolutionGenerator[] = {
    "TXZSolutionGenerator\0\0onTimeout()\0s\0"
    "onSolutionGenerated(QString)\0"
};

const QMetaObject TXZSolutionGenerator::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_TXZSolutionGenerator,
      qt_meta_data_TXZSolutionGenerator, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &TXZSolutionGenerator::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *TXZSolutionGenerator::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *TXZSolutionGenerator::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_TXZSolutionGenerator))
        return static_cast<void*>(const_cast< TXZSolutionGenerator*>(this));
    return QObject::qt_metacast(_clname);
}

int TXZSolutionGenerator::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onTimeout(); break;
        case 1: onSolutionGenerated((*reinterpret_cast< QString(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
static const uint qt_meta_data_TXZLogic[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_TXZLogic[] = {
    "TXZLogic\0\0moveCntChanged()\0"
};

const QMetaObject TXZLogic::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_TXZLogic,
      qt_meta_data_TXZLogic, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &TXZLogic::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *TXZLogic::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *TXZLogic::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_TXZLogic))
        return static_cast<void*>(const_cast< TXZLogic*>(this));
    return QObject::qt_metacast(_clname);
}

int TXZLogic::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: moveCntChanged(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void TXZLogic::moveCntChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
static const uint qt_meta_data_MapDisplayPanel[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      21,   17,   16,   16, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MapDisplayPanel[] = {
    "MapDisplayPanel\0\0x,y\0tileClicked(int,int)\0"
};

const QMetaObject MapDisplayPanel::staticMetaObject = {
    { &QLabel::staticMetaObject, qt_meta_stringdata_MapDisplayPanel,
      qt_meta_data_MapDisplayPanel, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &MapDisplayPanel::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *MapDisplayPanel::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *MapDisplayPanel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MapDisplayPanel))
        return static_cast<void*>(const_cast< MapDisplayPanel*>(this));
    return QLabel::qt_metacast(_clname);
}

int MapDisplayPanel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QLabel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: tileClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void MapDisplayPanel::tileClicked(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_GameDlg[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
       9,    8,    8,    8, 0x08,
      32,   28,    8,    8, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_GameDlg[] = {
    "GameDlg\0\0onMoveCntChanged()\0val\0"
    "onScrollBarValueChanged(int)\0"
};

const QMetaObject GameDlg::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_GameDlg,
      qt_meta_data_GameDlg, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &GameDlg::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *GameDlg::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *GameDlg::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_GameDlg))
        return static_cast<void*>(const_cast< GameDlg*>(this));
    return QDialog::qt_metacast(_clname);
}

int GameDlg::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onMoveCntChanged(); break;
        case 1: onScrollBarValueChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
static const uint qt_meta_data_EditerDlg[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      19,   11,   10,   10, 0x08,
      45,   41,   10,   10, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_EditerDlg[] = {
    "EditerDlg\0\0checked\0onButtonClicked(bool)\0"
    "x,y\0onTileClicked(int,int)\0"
};

const QMetaObject EditerDlg::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_EditerDlg,
      qt_meta_data_EditerDlg, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &EditerDlg::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *EditerDlg::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *EditerDlg::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_EditerDlg))
        return static_cast<void*>(const_cast< EditerDlg*>(this));
    return QDialog::qt_metacast(_clname);
}

int EditerDlg::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onButtonClicked((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: onTileClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
static const uint qt_meta_data_ReplayDlg[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      19,   11,   10,   10, 0x08,
      49,   45,   10,   10, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_ReplayDlg[] = {
    "ReplayDlg\0\0checked\0onLoadButtonClicked(bool)\0"
    "val\0onScrollBarValueChanged(int)\0"
};

const QMetaObject ReplayDlg::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_ReplayDlg,
      qt_meta_data_ReplayDlg, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &ReplayDlg::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *ReplayDlg::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *ReplayDlg::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_ReplayDlg))
        return static_cast<void*>(const_cast< ReplayDlg*>(this));
    return QDialog::qt_metacast(_clname);
}

int ReplayDlg::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onLoadButtonClicked((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: onScrollBarValueChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
