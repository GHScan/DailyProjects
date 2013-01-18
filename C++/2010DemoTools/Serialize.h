#pragma once

#include "SerializeFile.h"
#include "SerializeSerializeable.h"
#include "SerializeSimple.h"
#include "SerializeContainer.h"

/*
    ���л�ģ��ʹ��˵��

    1. �ļ������ַ�ʽ������ʹ��:
    ISerializeFile* p = createObject<ISerializeFile, SerializeXmlFileIn>();
    p->open();

    2. 
    ���� >>, д�� << ; ���߶���&

    3. 
    ��Ҫ�����л������ͷ�Ϊ3��:
    (1)������: ��int, char��pod, �Լ������stl�е�string;���ڲ�֧��<<�ȵ�
    ���Ϳ���ͨ��SCAN_DISABLE_STRING_SERIALIZE��ʹ����ͨ��, �Ӷ���ʹ��xml���л�
    (2)ISerializeable������, ���û���onSerialize��ʵ�ִ��л�����; ע��̳�
    �Ѿ���ISerializeable������ʱ, �����getClassVersion���д�ɸ����version
    +�����version; ��Ϊ����Ĵ��л��������ٱ�����, ����������Ҫ����
    (3)����; ֧������Ͳ���stl���м���������

    ���ڲ��ɸ��ĵ���, ��Ҫ���л�֧��, ���Բ������¼��ַ���:
    (1)��������������, ʵ��StringSerializeMethod����BinarySerializeMethod
    (2)��������ISerializeable�İ�װ
    (3)��д���л��ļ���<<, >>, &��

    4. 
    ����ָ��, ��д�ļ���ʱ����Ҫ�ǿ�; �ڶ��ļ���ʱ��, ���Ϊ��, �ᴴ��, ע��
    �ͷű���й©

    5. 
    ISerializeable���л��Ķ�̬֧�ִ�Factory�л�ȡ; ��������Ҫ����;(��Ҫ��ȫ�ֿռ���)
*/