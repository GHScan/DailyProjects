#pragma once

#include "SerializeFile.h"
#include "SerializeSerializeable.h"
#include "SerializeSimple.h"
#include "SerializeContainer.h"

/*
    串行化模块使用说明

    1. 文件以这种方式创建和使用:
    ISerializeFile* p = createObject<ISerializeFile, SerializeXmlFileIn>();
    p->open();

    2. 
    读用 >>, 写用 << ; 或者都用&

    3. 
    需要被串行化的类型非为3种:
    (1)简单类型: 如int, char等pod, 以及特殊的stl中的string;对于不支持<<等的
    类型可以通过SCAN_DISABLE_STRING_SERIALIZE来使编译通过, 从而不使用xml串行化
    (2)ISerializeable派生类, 由用户在onSerialize中实现串行化方法; 注意继承
    已经是ISerializeable的类型时, 子类的getClassVersion最好写成父类的version
    +子类的version; 因为父类的串行化方法不再被调用, 所以子类需要负责
    (3)容器; 支持数组和部分stl序列及关联容器

    对于不可更改的类, 需要串行化支持, 可以采用以下几种方法:
    (1)把它当做简单类型, 实现StringSerializeMethod或者BinarySerializeMethod
    (2)给它加上ISerializeable的包装
    (3)重写串行化文件的<<, >>, &等

    4. 
    对于指针, 在写文件的时候需要非空; 在读文件的时候, 如果为空, 会创建, 注意
    释放避免泄漏

    5. 
    ISerializeable串行化的多态支持从Factory中获取; 即类型需要声明;(需要在全局空间中)
*/