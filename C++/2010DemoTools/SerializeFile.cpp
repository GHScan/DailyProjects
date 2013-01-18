#include "StdAfx.h"

#define TIXML_USE_STL
#include <tinyxml/_tinyxml.h>

#include "SerializeFile.h"
#include "Factory.h"
#include "Utility.h"
#include "Exception.h"

#include "MemoryCheck.h"

#undef min
#undef max

namespace Scan
{
    namespace Serialize
    {
        /**
            @brief 对象的二进制串行化信息
        */
        struct ObjecBinarytInfo
        {
            uint32 objSize;
        };

        /**
            @brief 二进制输出
        */
        class SerializeBinaryOut:
            public ISerializeFile
        {
        public:
            SerializeBinaryOut():
              m_bufPtr(0)
              {
              }

        private:
            SerializeBinaryOut(const SerializeBinaryOut&);
            SerializeBinaryOut& operator = (const SerializeBinaryOut&);

        private:
            virtual void beginObject(const char *name)
            {
                ObjecBinarytInfo *info = writeNewObject();
                info->objSize = getBufSize();
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                ObjecBinarytInfo *info = getLastObject();

                uint32 objSize = p->getSize();
                extendBuf(objSize);
                p->saveToBytes(getCurrentBuf());
                movePtr(objSize);
            }

            virtual void handleSerializeable(ISerializeable*& p)
            {
                uint32 ver = p->getClassVersion();
                String objName = Factory::getValidDynamicClassName(p);
                SCAN_EXCP_VERIFY(!objName.empty(), ET_FatalError);

                uint32 size1 = sizeof(ver);
                uint32 size2 = (uint32)objName.size() + 1;
                extendBuf(size1 + size2);
                *(uint32*)getCurrentBuf() = ver;
                movePtr(size1);
                memcpy(getCurrentBuf(), objName.c_str(), size2);
                movePtr(size2);

                p->onSerialzie(*this);
            }

            virtual void endObject()
            {
                ObjecBinarytInfo *info = getLastObject();
                info->objSize = getBufSize() - info->objSize;
                m_objects.pop_back();

                tryflushBuf();
            }

            virtual bool isInputFile() const
            {
                return false;
            }

        protected:
            virtual uint32 writeToFile(const void *buf, uint32 bufSize) = 0;

        private:
            void extendBuf(uint32 bytes)
            {       
                m_buf.resize(m_buf.size() + bytes);
            }   

            void* getCurrentBuf()
            {
                return &m_buf[m_bufPtr];
            }

            void movePtr(uint32 offset)
            {
                m_bufPtr += offset;
            }

            void tryflushBuf()
            {
                if (!m_objects.empty())
                {
                    return;
                }
                m_bufPtr = 0;
                if (!m_buf.empty())
                {
                    writeToFile(&m_buf[0], static_cast<uint32>(m_buf.size()));
                    if (m_buf.size() > 1 << 20)
                    {
                        m_buf.swap(std::vector<char>());
                    }
                    else
                    {
                        m_buf.clear();
                    }
                }
            }

            ObjecBinarytInfo* writeNewObject()
            {
                extendBuf(sizeof(ObjecBinarytInfo));
                m_objects.push_back(m_bufPtr);
                m_bufPtr += sizeof(ObjecBinarytInfo);
                return getLastObject();
            }

            ObjecBinarytInfo* getLastObject()
            {
                return (ObjecBinarytInfo*)&m_buf[m_objects.back()];
            }

            uint32 getBufSize()
            {
                return (uint32)m_buf.size();
            }

        private:
            std::vector<char>           m_buf;
            uint32                      m_bufPtr;

            std::vector<uint32>         m_objects;
        };

        /**
            @brief 二进制文件输出
        */
        class SerializeBinaryFileOut:
            public SerializeBinaryOut
        {
        private:
            ~SerializeBinaryFileOut()
            {
                close();
            }

            virtual bool isOpened() const
            {
                return m_file ? true : false;
            }

            virtual bool open(const String& name)
            {
                close();

                {
                    NativeLocale l;
                    m_file.open(name.c_str(), std::ios::binary);
                }
                
                if (isOpened())
                {
                    m_name = name;
                    return true;
                }
                return false;
            }

            virtual void close()
            {
                m_name.clear();
                m_file.close();
                m_file.clear();
            }

            virtual const String& getName() const 
            {
                return m_name;
            }

            virtual uint32 writeToFile(const void *buf, uint32 bufSize)
            {
                m_file.write(static_cast<const char*>(buf), bufSize);
                return bufSize;
            }

        private:
            String      m_name;
            OFStream    m_file;
        };

        /**
            @brief 二进制数据输出
            输出到std::vector<char>
        */
        class SerializeBinaryDataOut:
            public SerializeBinaryOut
        {
        public:
            SerializeBinaryDataOut():
              m_data(NULL){}

            ~SerializeBinaryDataOut()
            {
                close();
            }

            virtual bool isOpened() const
            {
                return m_data != NULL;
            }

            virtual bool open(const String& name)
            {
                close();

                fromString(m_data, name);
                return isOpened();
            }

            virtual void close()
            {
                m_data = NULL;
            }

            virtual const String& getName() const 
            {
                return getEmptyString();
            }

            virtual uint32 writeToFile(const void *buf, uint32 bufSize)
            {
                uint32 lastSize = static_cast<uint32>(m_data->size());
                m_data->resize(lastSize + bufSize);
                memcpy(&(*m_data)[lastSize], buf, bufSize);
                return bufSize;
            }

        private:
            std::vector<char>   *m_data;
        };

        /**
            @brief 二进制输入
        */
        class SerializeBinaryIn:
            public ISerializeFile
        {
        public:
            SerializeBinaryIn():
              m_bufPtr(0)
              {
              }

        private:
            SerializeBinaryIn(const SerializeBinaryIn&);
            SerializeBinaryIn& operator = (const SerializeBinaryIn&);

        private:
            virtual void beginObject(const char *name)
            {
                if (m_objects.empty())
                {
                    readToNewBuf(sizeof(ObjecBinarytInfo));
                    ObjecBinarytInfo *p = addNewObject();
                    readToBuf(p->objSize);
                }
                else
                {
                    addNewObject();
                }
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                ObjecBinarytInfo *info = getLastObject();
                p->loadFromBytes(getCurrentBuf());
                movePtr(info->objSize);
            }

            virtual void handleSerializeable(ISerializeable *&p)
            {
                ObjecBinarytInfo *info = getLastObject();

                uint32 ver;
                String objName;

                uint32 size1 = sizeof(ver);
                uint32 size2 = (uint32)strlen((char*)getCurrentBuf() + size1);
                objName.resize(size2);
                ver = *(uint32*)getCurrentBuf();
                memcpy((char*)objName.c_str(), (char*)getCurrentBuf() + size1, size2);

                if (p == NULL)
                {
                    Factory::createObject(p, objName);
                    SCAN_EXCP_VERIFY(p != NULL, ET_FatalError);
                }

                if (p->getClassVersion() == ver)
                {
                    movePtr(size1 + size2 + 1);

                    p->onSerialzie(*this);
                    return;
                }

                movePtr(info->objSize);
            }

            virtual void endObject()
            {
                m_objects.pop_back();
                tryClearBuf();
            }

            virtual bool isInputFile() const
            {
                return true;
            }

        protected:
            virtual uint32 readFromFile(void* buf, uint32 requireSize) = 0;

        private:

            void readToNewBuf(uint32 bytes)
            {
                m_bufPtr = 0;
                m_buf.resize(bytes);
                readFromFile(&m_buf[0], bytes);
            }

            void readToBuf(uint32 bytes)
            {
                size_t start = m_buf.size();
                m_buf.resize(start + bytes);
                readFromFile(&m_buf[start], bytes);
            }

            void tryClearBuf()
            {
                if (!m_objects.empty())
                {
                    return;
                }
                if (m_buf.size() > 1 << 20)
                {
                    m_buf.swap(std::vector<char>());
                }
                else
                {
                    m_buf.clear();
                }
            }

            ObjecBinarytInfo* addNewObject()
            {
                m_objects.push_back(m_bufPtr);
                m_bufPtr += sizeof(ObjecBinarytInfo);
                return getLastObject();
            }

            ObjecBinarytInfo* getLastObject()
            {
                return (ObjecBinarytInfo*)&m_buf[m_objects.back()];
            }

            void* getCurrentBuf()
            {
                return &m_buf[m_bufPtr];
            }

            void movePtr(uint32 offset)
            {
                m_bufPtr += offset;
            }

        private:
            std::vector<char>           m_buf;
            uint32                      m_bufPtr;

            std::vector<uint32>         m_objects;
        };

        /**
            @brief 二进制文件输入
        */
        class SerializeBinaryFileIn:
            public SerializeBinaryIn
        {
        public:
            virtual bool isOpened() const
            {
                return m_file ? true : false;
            }

            virtual bool open(const String& name)
            {
                close();

                {
                    NativeLocale l;
                    m_file.open(name.c_str(), std::ios::binary);
                }

                if (isOpened())
                {
                    m_name = name;
                    return true;
                }
                return false;
            }

            virtual void close()
            {
                m_name.clear();
                m_file.close();
                m_file.clear();
            }

            virtual const String& getName() const 
            {
                return m_name;
            }

            virtual uint32 readFromFile(void* buf, uint32 requireSize)
            {
                m_file.read(static_cast<char*>(buf), requireSize);
                return m_file.gcount();
            }

        private:
            String      m_name;
            IFStream    m_file;
        };

        /**
            @brief 二进制数据输入
            从std::vector<char>中输入
        */
        class SerializeBinaryDataIn:
            public SerializeBinaryIn
        {
        public:
            SerializeBinaryDataIn():
              m_data(NULL){}

            virtual bool isOpened() const
            {
                return m_data != NULL;
            }

            virtual bool open(const String& name)
            {
                close();

                fromString(m_data, name);
                return isOpened();
            }

            virtual void close()
            {
                m_readPtr = 0;
                m_data = NULL;
            }

            virtual const String& getName() const 
            {
                return getEmptyString();
            }

            virtual uint32 readFromFile(void* buf, uint32 requireSize)
            {
                uint32 leftSize = static_cast<uint32>(m_data->size()) - m_readPtr;
                uint32 readSize = (uint32)std::min(leftSize, requireSize);
                memcpy(buf, &(*m_data)[m_readPtr], readSize);
                m_readPtr += readSize;
                return readSize;
            }

        private:
            std::vector<char>       *m_data;
            uint32      m_readPtr;
        };

        static const char * XML_COMMENT = "ScanXmlSerializeFile impl with tinyxml";
        static const char * XML_CLASS_VER = "ver";
        static const char * XML_CLASS_NAME = "type";

        /**
            @brief xml输出
        */
        class SerializeXmlOut:
            public ISerializeFile
        {
        public:
            SerializeXmlOut():
              m_isOpened(false), m_curNode(NULL) {}

        private:
            SerializeXmlOut(const SerializeXmlOut&);
            SerializeXmlOut& operator = (const SerializeXmlOut&);

        private:
            virtual bool isOpened() const
            {
                return m_isOpened;
            }

            virtual bool open(const String& name)
            {
                close();

                setString(name);
                m_isOpened = true;
                addHeader();
                m_curNode = &m_doc;

                return true;
            }

            virtual bool isInputFile() const
            {
                return false;
            }

            virtual const String& getName() const 
            {
                return getString();
            }

            virtual void beginObject(const char *name)
            {
                assert(name != NULL && name[0] != 0);

                TiXmlElement *elem = new TiXmlElement(name);
                m_curNode->LinkEndChild(elem);
                m_curNode = elem;
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                TiXmlElement *elem = m_curNode->ToElement();
                String s; 
                p->saveToString(s);
                elem->LinkEndChild(new TiXmlText(s));
            }

            virtual void handleSerializeable(ISerializeable *&p)
            {
                TiXmlElement *elem = m_curNode->ToElement();

                uint16 ver = p->getClassVersion();
                String objName = Factory::getValidDynamicClassName(p);
                SCAN_EXCP_VERIFY(!objName.empty(), ET_FatalError);

                elem->SetAttribute(XML_CLASS_VER, ver);
                elem->SetAttribute(XML_CLASS_NAME, objName.c_str());

                p->onSerialzie(*this);
            }

            virtual void endObject()
            {
                m_curNode = m_curNode->Parent();
            }

        protected:
            virtual void setString(const String& name) = 0;
            virtual const String& getString() const = 0;
            virtual bool saveDocument(const TiXmlDocument* doc) = 0;

            virtual void close()
            {
                if (isOpened())
                {
                    saveDocument(&m_doc);
                    m_doc.Clear();

                    m_isOpened = false;
                    m_curNode = NULL;
                }
            }

        private:
            void addHeader()
            {
                TiXmlDeclaration *decl = new TiXmlDeclaration(
                    "1.0",
                    "gb2312",
                    "yes");
                m_doc.LinkEndChild(decl);

                TiXmlComment *comment = new TiXmlComment(XML_COMMENT);
                m_doc.LinkEndChild(comment);
            }

        private:
            TiXmlNode          *m_curNode;
            TiXmlDocument       m_doc;
            bool                m_isOpened;
        };

        /** 
            @brief xml文件输出
        */
        class SerializeXmlFileOut:
            public SerializeXmlOut
        {
        public:
            ~SerializeXmlFileOut()
            {
                close();
            }

            virtual void setString(const String& name)
            {
                m_fileName = name;
            }

            virtual const String& getString() const
            {
                return m_fileName;
            }

            virtual bool saveDocument(const TiXmlDocument* doc)
            {
                return doc->SaveFile(m_fileName.c_str());
            }

        private:
            String      m_fileName;
        };

        /**
            @brief xml字符串输出
        */
        class SerializeXmlStringOut:
            public SerializeXmlOut
        {
        public:
            ~SerializeXmlStringOut()
            {
                close();
            }

            SerializeXmlStringOut():
              m_userDest(NULL){}

            virtual void setString(const String& name)
            {
                m_userDest = &const_cast<String&>(name);
            }

            virtual const String& getString() const
            {
                return getEmptyString();
            }

            virtual bool saveDocument(const TiXmlDocument* doc)
            {
                *m_userDest = toString(*doc);
                return !doc->Error();
            }

        private:
            String      *m_userDest;
        };

        /**
            @brief xml输入
        */
        class SerializeXmlIn:
            public ISerializeFile
        {
        public:
            SerializeXmlIn():
              m_isOpened(false){}

        private:
            SerializeXmlIn(const SerializeXmlIn&);
            SerializeXmlIn& operator = (const SerializeXmlIn&);

        private:
            virtual bool isOpened() const
            {
                return m_isOpened;
            }

            virtual bool open(const String& name)
            {
                close();

                setString(name);
                m_isOpened = loadDocument(&m_doc);
                if (m_isOpened)
                {
                    checkHeader();
                    m_curNode = &m_doc;
                }
                return m_isOpened;
            }

            virtual void close()
            {
                if (m_isOpened)
                {
                    setString("");
                    m_isOpened = false;
                    m_doc.Clear();
                    m_curNode = NULL;
                }
            }

            virtual bool isInputFile() const
            {
                return true;
            }

            virtual const String& getName() const 
            {
                return getString();
            }

            virtual void beginObject(const char *name)
            {
                assert(name != NULL && name[0] != 0);

                m_curNode = m_curNode->FirstChild();
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                TiXmlElement *elem = m_curNode->ToElement();

                const char *cstr = elem->GetText();
                String s(cstr == NULL ? "" : cstr);
                p->loadFromString(s);
            }

            virtual void handleSerializeable(ISerializeable *&p)
            {
                TiXmlElement *elem = m_curNode->ToElement();

                int ver = -1;
                String objName;
                int queryResult = elem->QueryIntAttribute(XML_CLASS_VER, &ver);
                if (queryResult == TIXML_SUCCESS)
                {
                    const char* cstr = elem->Attribute(XML_CLASS_NAME);
                    objName = cstr == NULL ? "" : cstr;
                }
                if (queryResult == TIXML_SUCCESS && !objName.empty())
                {
                    if (p == NULL)
                    {
                        Factory::createObject(p, objName);
                        SCAN_EXCP_VERIFY(p != NULL, ET_FatalError);
                    }                    
                    if (p != NULL && p->getClassVersion() == ver)
                    {
                        p->onSerialzie(*this);
                    }
                }
            }

            virtual void endObject()
            {
                m_curNode = m_curNode->Parent();
                m_curNode->RemoveChild(m_curNode->FirstChild());
            }

        protected:
            virtual void setString(const String& name) = 0;
            virtual const String& getString() const = 0;
            virtual bool loadDocument(TiXmlDocument* doc) = 0;

        private:
            void checkHeader()
            {
                TiXmlComment *comment = m_doc.FirstChild()->NextSibling()->ToComment();
                assert(comment->ValueStr() == XML_COMMENT);
                m_doc.RemoveChild(m_doc.FirstChild());
                m_doc.RemoveChild(m_doc.FirstChild());
            }

        private:
            TiXmlDocument               m_doc;
            TiXmlNode                  *m_curNode;
            bool                        m_isOpened;
        };
    
        /**
            @brief xml文件输入
        */
        class SerializeXmlFileIn:
            public SerializeXmlIn
        {
        public:
            virtual void setString(const String& name)
            {
                m_fileName = name;
            }

            virtual const String& getString() const
            {
                return m_fileName;
            }

            virtual bool loadDocument(TiXmlDocument* doc) 
            {
                return doc->LoadFile(m_fileName);
            }

        private:
            String      m_fileName;
        };

        /**
            @brief xml字符串输入
        */
        class SerializeXmlStringIn:
            public SerializeXmlIn
        {
        public:
            virtual void setString(const String& name)
            {
                m_content = name;
            }

            virtual const String& getString() const
            {
                return getEmptyString();
            }

            virtual bool loadDocument(TiXmlDocument* doc) 
            {
                doc->Parse(m_content.c_str());
                return !doc->Error();
            }

        private:
            String      m_content;
        };
    }
}

SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeBinaryFileIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeBinaryFileOut);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeBinaryDataIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeBinaryDataOut);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlFileIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlFileOut);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlStringIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlStringOut);