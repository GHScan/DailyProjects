/****************************************************************************
** Meta object code from reading C++ file 'Console.cpp'
**
** Created: Thu Apr 14 12:59:07 2011
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
static const uint qt_meta_data_DrawPanel[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      13,   11,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      49,   39,   10,   10, 0x0a,
      90,   80,   10,   10, 0x0a,
     125,  121,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_DrawPanel[] = {
    "DrawPanel\0\0s\0luaErrorDetected(QString)\0"
    "minX,maxX\0onXRangeChanged(double,double)\0"
    "minY,maxY\0onYRangeChanged(double,double)\0"
    "exp\0onExpressionChanged(QString)\0"
};

const QMetaObject DrawPanel::staticMetaObject = {
    { &QLabel::staticMetaObject, qt_meta_stringdata_DrawPanel,
      qt_meta_data_DrawPanel, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &DrawPanel::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *DrawPanel::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *DrawPanel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_DrawPanel))
        return static_cast<void*>(const_cast< DrawPanel*>(this));
    return QLabel::qt_metacast(_clname);
}

int DrawPanel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QLabel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: luaErrorDetected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: onXRangeChanged((*reinterpret_cast< double(*)>(_a[1])),(*reinterpret_cast< double(*)>(_a[2]))); break;
        case 2: onYRangeChanged((*reinterpret_cast< double(*)>(_a[1])),(*reinterpret_cast< double(*)>(_a[2]))); break;
        case 3: onExpressionChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void DrawPanel::luaErrorDetected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MainDlg[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: signature, parameters, type, tag, flags
      19,    9,    8,    8, 0x05,
      58,   48,    8,    8, 0x05,
      91,   87,    8,    8, 0x05,

 // slots: signature, parameters, type, tag, flags
     118,    8,    8,    8, 0x0a,
     144,    8,    8,    8, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MainDlg[] = {
    "MainDlg\0\0minX,maxX\0xRangeChanged(double,double)\0"
    "minY,maxY\0yRangeChanged(double,double)\0"
    "exp\0expressionChanged(QString)\0"
    "onRangeEdit_textChanged()\0"
    "onApplyExpButton_Clicked()\0"
};

const QMetaObject MainDlg::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_MainDlg,
      qt_meta_data_MainDlg, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &MainDlg::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *MainDlg::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *MainDlg::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MainDlg))
        return static_cast<void*>(const_cast< MainDlg*>(this));
    return QDialog::qt_metacast(_clname);
}

int MainDlg::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: xRangeChanged((*reinterpret_cast< double(*)>(_a[1])),(*reinterpret_cast< double(*)>(_a[2]))); break;
        case 1: yRangeChanged((*reinterpret_cast< double(*)>(_a[1])),(*reinterpret_cast< double(*)>(_a[2]))); break;
        case 2: expressionChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: onRangeEdit_textChanged(); break;
        case 4: onApplyExpButton_Clicked(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void MainDlg::xRangeChanged(double _t1, double _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MainDlg::yRangeChanged(double _t1, double _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MainDlg::expressionChanged(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE
