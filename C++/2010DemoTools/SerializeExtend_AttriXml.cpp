#include "stdafx.h"

#define TIXML_USE_STL
#include <tinyxml/_tinyxml.h>

#include "SerializeFile.h"
#include "Factory.h"

#include "SerializeExtend_AttriXml.h"

#include "MemoryCheck.h"

namespace Scan
{
    namespace Serialize
    {
        static const char * XML_COMMENT = "ScanXmlSerializeFile impl with tinyxml";
        static const char * XML_CLASS_VER = "ver";
        static const char * XML_CLASS_NAME = "type";

        /**
            @brief xml输出
        */
        class SerializeXmlAttribOut:
            public ISerializeFile
        {
        public:
            SerializeXmlAttribOut():
              m_isOpened(false), m_curNode(NULL), m_isLastSimple(false) {}

        private:
            SerializeXmlAttribOut(const SerializeXmlAttribOut&);
            SerializeXmlAttribOut& operator = (const SerializeXmlAttribOut&);

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

                m_curNode->LinkEndChild(new TiXmlElement(""));
                m_curNode = m_curNode->LastChild();
                m_curNode->SetValue(name);
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                TiXmlNode *node = m_curNode;
                String lastName = node->Value();
                m_curNode = m_curNode->Parent();
                m_curNode->RemoveChild(node);

                m_isLastSimple = true;

                TiXmlElement *elem = m_curNode->ToElement();
                String s; 
                p->saveToString(s);
                elem->SetAttribute(lastName, s);
            }

            virtual void handleSerializeable(ISerializeable *&p)
            {
                p->onSerialzie(*this);
            }

            virtual void endObject()
            {
                if (m_isLastSimple)
                {
                    m_isLastSimple = false;
                }
                else
                {
                    m_curNode = m_curNode->Parent();
                }
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
            bool                m_isLastSimple;
        };

        /** 
            @brief xml文件输出
        */
        class SerializeXmlAttribFileOut:
            public SerializeXmlAttribOut
        {
        public:
            ~SerializeXmlAttribFileOut()
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
        class SerializeXmlAttribStringOut:
            public SerializeXmlAttribOut
        {
        public:
            ~SerializeXmlAttribStringOut()
            {
                close();
            }

            SerializeXmlAttribStringOut():
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
        class SerializeXmlAttribIn:
            public ISerializeFile
        {
        public:
            SerializeXmlAttribIn():
              m_isOpened(false), m_isLastSimple(false), m_advance(false){}

        private:
            SerializeXmlAttribIn(const SerializeXmlAttribIn&);
            SerializeXmlAttribIn& operator = (const SerializeXmlAttribIn&);

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

                m_lastName = name;
                m_advance = m_curNode->FirstChild() != NULL;
                if (m_advance)
                {
                    m_curNode = m_curNode->FirstChild();
                }
            }

            virtual void handleSimpleObject(ISimpleObject *&p)
            {
                m_isLastSimple = true;
                if (m_advance)
                {
                    m_curNode = m_curNode->Parent();
                }

                TiXmlElement *elem = m_curNode->ToElement();

                const char *attriV = elem->Attribute(m_lastName.c_str());
                if (attriV != NULL)
                {
                    p->loadFromString(attriV);
                }
                else
                {
                    abort();
                }
            }

            virtual void handleSerializeable(ISerializeable *&p)
            {
                p->onSerialzie(*this);
            }

            virtual void endObject()
            {
                if (!m_isLastSimple)
                {
                    m_curNode = m_curNode->Parent();
                    m_curNode->RemoveChild(m_curNode->FirstChild());
                }

                m_isLastSimple = false;
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
            String                      m_lastName;
            bool                        m_isLastSimple;
            bool                        m_advance;
        };

        /**
            @brief xml文件输入
        */
        class SerializeXmlAttribFileIn:
            public SerializeXmlAttribIn
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
        class SerializeXmlAttribStringIn:
            public SerializeXmlAttribIn
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

SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlAttribFileIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlAttribFileOut);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlAttribStringIn);
SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeFile, Scan::Serialize::SerializeXmlAttribStringOut);