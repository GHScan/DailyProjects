using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Winista.Text.HtmlParser.Nodes;
using Winista.Text.HtmlParser;
using System.Xml;
using Winista.Text.HtmlParser.Filters;
using Winista.Text.HtmlParser.Util;
using System.Collections;
using System.Text.RegularExpressions;
using System.IO;

namespace CSharp08
{
    partial class Program
    {
        /// <summary>
        /// 解析Xml文件的帮助类
        /// </summary>
        public class XMLHelper
        {
            /// <summary>
            /// 有效名称的正则表达式
            /// </summary>
            static string validName = @"^[^\$\/;""\!#\)\.]+$";

            #region CovertHtmlToXml
            /// <summary>
            /// 转换html源码为xml格式
            /// </summary>
            /// <param name="html">html源码</param>
            /// <returns>xml字符串</returns>
            /// <param name="TargetTag">需转换的标记名</param>
            public static string CovertHtmlToXml(string html, string targetTag)
            {
                try
                {
                    XmlDocument doc = new XmlDocument();
                    XmlNode xmlDeclaration = doc.CreateXmlDeclaration("1.0", "utf-8", null);
                    doc.AppendChild(xmlDeclaration);

                    // 借助htmlparser解析html内容
                    Parser parser = Parser.CreateParser(html, "GBK");
                    // 筛选出指定的节点
                    TagNameFilter tnf = new TagNameFilter(targetTag);
                    NodeList nodes = parser.Parse(tnf);

                    // 创建根节点
                    XmlElement root = doc.CreateElement("Tags");

                    TagNode tagNode = null;
                    Hashtable ht = null;
                    XmlAttribute attr = null;
                    XmlElement parent = null;
                    for (int i = 0; i < nodes.Size(); i++)
                    {
                        tagNode = nodes[i] as TagNode;
                        parent = doc.CreateElement(tagNode.TagName);

                        // 添加属性
                        ht = tagNode.Attributes;
                        foreach (DictionaryEntry ent in ht)
                        {
                            // 查看属性名是否合法
                            if (Regex.IsMatch(ent.Key.ToString(), validName))
                            {
                                attr = doc.CreateAttribute(ent.Key.ToString());
                                attr.Value = ent.Value.ToString();
                                parent.Attributes.Append(attr);
                            }
                        }// end foreach (DictionaryEntry ent in ht)

                        AppendChild(tagNode, parent, doc);

                        root.AppendChild(parent);
                    }
                    doc.AppendChild(root);

                    return doc.OuterXml;

                    //throw new Exception("给定的html文本必须至少包含一个" + targetTag + "节点");
                }
                catch (Exception ex)
                {
                    throw new Exception("转换html内容出错:" + ex.Message);
                }
            }

            /// <summary>
            /// 添加子节点
            /// </summary>
            /// <param name="tagNode">Html的父节点</param>
            /// <param name="parent">Xml的父节点</param>
            /// <param name="doc">Xml文档对象</param>
            private static void AppendChild(INode tagNode, XmlNode parent, XmlDocument doc)
            {
                INode node = null;
                XmlNode xmlNode = null;
                XmlAttribute attr = null;
                Hashtable ht = null;

                // 判断是否包含子节点
                if (tagNode.Children != null && tagNode.Children.Size() > 0)
                {
                    for (int i = 0; i < tagNode.Children.Size(); i++)
                    {
                        node = tagNode.Children[i];
                        xmlNode = null;
                        attr = null;
                        ht = null;

                        // 如果是html标记节点
                        if (node is TagNode)
                        {
                            TagNode tn = node as TagNode;
                            if (Regex.IsMatch(tn.TagName, validName))
                            {
                                xmlNode = doc.CreateElement(tn.TagName);

                                // 添加属性
                                ht = tn.Attributes;
                                foreach (DictionaryEntry ent in ht)
                                {
                                    // 查看属性名是否合法
                                    if (Regex.IsMatch(ent.Key.ToString(), validName))
                                    {
                                        attr = doc.CreateAttribute(ent.Key.ToString());
                                        attr.Value = ent.Value.ToString();
                                        xmlNode.Attributes.Append(attr);
                                    }
                                }
                            }
                        }

                        // 如果是文本节点
                        if (node is TextNode)
                        {
                            xmlNode = doc.CreateTextNode((node as TextNode).ToPlainTextString());
                        }

                        if (xmlNode != null)
                        {
                            parent.AppendChild(xmlNode);
                            AppendChild(node, xmlNode, doc);
                        }
                    }
                }
            }
            #endregion
        }

        static void Main(string[] args)
        {
            if (args.Length < 2)
            {
                print("参数个数不够: 参数0-源文件, 参数1-目标文件");
                return;
            }

            using (StreamReader reader = new StreamReader(args[0], Encoding.Default))
            using (StreamWriter writer = new StreamWriter(args[1], false, Encoding.Default))
            {
                writer.Write(
                    XMLHelper.CovertHtmlToXml(reader.ReadToEnd(), "html").Replace(
                    "<?xml version=\"1.0\" encoding=\"utf-8\"?>", "<?xml version=\"1.0\" encoding=\"gb2312\"?>"));
            }
        }
    }
}