using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Reflection;
using System.Diagnostics;

public static class ReferenceAnalysisAlgorithm
{
    public class ReflectionPolicy
    {
        public Func<Type, IEnumerable<FieldInfo>> GetStaticFields;
        public Func<Type, IEnumerable<PropertyInfo>> GetStaticProperties;
        public Func<Type, IEnumerable<FieldInfo>> GetInstanceFields;
        public Func<Type, IEnumerable<PropertyInfo>> GetInstanceProperties;
        public Func<Type, IEnumerable<Type>> GetInstanceMemberTypes;
        public Func<Type, IEnumerable<Type>> GetInstanceSuperTypes;
        public Func<Assembly, IEnumerable<Type>> GetAssemblyTypes;
        public static bool IsAtom(Type type)
        {
            return type.IsPrimitive || type.IsEnum;
        }
        public ReflectionPolicy()
        {
            GetAssemblyTypes = assembly =>
            {
                return
                    from type in assembly.GetTypes()
                    where !type.ContainsGenericParameters && !type.IsEnum
                    select type;
            };
            GetStaticFields = type =>
            {
                return
                    from field in type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.FlattenHierarchy)
                    where !field.FieldType.ContainsGenericParameters
                    select field;
            };
            GetStaticProperties = type =>
            {
                return
                    from prop in type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.FlattenHierarchy)
                    where !prop.PropertyType.ContainsGenericParameters
                    let getter = prop.GetGetMethod()
                    where getter != null && getter.GetParameters().Length == 0
                    select prop;
            };
            GetInstanceFields = type =>
            {
                if (type.IsArray) return Enumerable.Empty<FieldInfo>();
                // Skip classes in System.Reflection, to avoid illegal field access
                if (type.Namespace == kSystemReflectionNameSpace) return Enumerable.Empty<FieldInfo>();
                return
                    from field in type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
                    where !field.FieldType.ContainsGenericParameters
                    select field;
            };
            GetInstanceProperties = type =>
            {
                if (type.IsArray) return Enumerable.Empty<PropertyInfo>();
                // Skip classes in System.Reflection, to avoid illegal property access
                if (type.Namespace == kSystemReflectionNameSpace) return Enumerable.Empty<PropertyInfo>();
                return
                    from prop in type.GetProperties(BindingFlags.Public | BindingFlags.Instance)
                    where !prop.PropertyType.ContainsGenericParameters
                    let getter = prop.GetGetMethod()
                    where getter != null && getter.GetParameters().Length == 0
                    select prop;
            };
            GetInstanceMemberTypes = _GetInstanceMemberTypes;
            GetInstanceSuperTypes = _GetInstanceSuperTypes;
        }
        private IEnumerable<Type> _GetInstanceMemberTypes(Type type)
        {
            if (type.IsArray) yield return type.GetElementType();
            else
            {
                foreach (var field in GetInstanceFields(type)) yield return field.FieldType;
                foreach (var prop in GetInstanceProperties(type)) yield return prop.PropertyType;
            }
        }
        private static IEnumerable<Type> _GetInstanceSuperTypes(Type type)
        {
            while (type != null)
            {
                yield return type;
                foreach (var itype in type.GetInterfaces()) yield return itype;
                type = type.BaseType;
            }
        }
        private static string kSystemReflectionNameSpace = typeof(System.Reflection.MethodInfo).Namespace;
    }
    private class TypeLayout
    {
        public PropertyInfo[] Properties;
        public FieldInfo[] Fields;
    }
    private class TypeNode
    {
        public TypeLayout Layout;
        public bool Related;
        public List<TypeNode> DerivedNodes = new List<TypeNode>();
        public void AddDerivedNode(TypeNode node)
        {
            if (!DerivedNodes.Contains(node)) DerivedNodes.Add(node);
        }
    }
    private class TypeGraph
    {
        public TypeGraph(Type targetType, IEnumerable<Type> types, ReflectionPolicy policy)
        {
            mPolicy = policy;

            foreach (var type in types) GetOrAddNode(type);

            var node = GetOrAddNode(targetType);
            node.Related = true;
            mNewRelatedNodes.Push(node);
            MarkDerivedNodesAsRelated();
        }

        public TypeLayout GetLayout(Type type)
        {
            var node = GetOrAddNode(type);
            MarkDerivedNodesAsRelated();
            if (node.Related && node.Layout == null)
            {
                node.Layout = BuildLayout(type);
            }
            return node.Layout;
        }
        private TypeNode GetOrAddNode(Type type)
        {
            TypeNode node;
            if (mType2Node.TryGetValue(type, out node)) return node;
            node = new TypeNode();
            mType2Node.Add(type, node);

            foreach (var memberType in mPolicy.GetInstanceMemberTypes(type))
            {
                var memberNode = GetOrAddNode(memberType);
                memberNode.AddDerivedNode(node);
                node.Related |= memberNode.Related;
            }
            foreach (var superType in mPolicy.GetInstanceSuperTypes(type))
            {
                node.AddDerivedNode(GetOrAddNode(superType));
            }

            if (node.Related) mNewRelatedNodes.Push(node);

            return node;
        }
        private TypeLayout BuildLayout(Type type)
        {
            if (type.IsArray || ReflectionPolicy.IsAtom(type))
            {
                return new TypeLayout();
            }
            else
            {
                return new TypeLayout()
                {
                    Fields = mPolicy.GetInstanceFields(type).Where(f => mType2Node[f.FieldType].Related).ToArray(),
                    Properties = mPolicy.GetInstanceProperties(type).Where(p => mType2Node[p.PropertyType].Related).ToArray(),
                };
            }
        }
        private void MarkDerivedNodesAsRelated()
        {
            while (mNewRelatedNodes.Count > 0)
            {
                var node = mNewRelatedNodes.Pop();
                foreach (var dnode in node.DerivedNodes)
                {
                    dnode.Layout = null;
                    if (!dnode.Related)
                    {
                        dnode.Related = true;
                        mNewRelatedNodes.Push(dnode);
                    }
                }
            }
        }

        private Dictionary<Type, TypeNode> mType2Node = new Dictionary<Type, TypeNode>();
        private Stack<TypeNode> mNewRelatedNodes = new Stack<TypeNode>();
        private ReflectionPolicy mPolicy;
    }
    private class ScopeTimer : IDisposable
    {
        public ScopeTimer(string name, Action<string> log)
        {
            mName = name;
            mLog = log;
            mStopwatch = new Stopwatch();
            mStopwatch.Start();
        }
        public void Dispose()
        {
            mStopwatch.Stop();
            mLog(string.Format("{0}: {1}", mName, mStopwatch.Elapsed));
        }
        private string mName;
        private Action<string> mLog;
        private Stopwatch mStopwatch;
    }

    public class ReferenceFinder
    {
        public Action<string> LogInfo = Console.Out.WriteLine;
        public Action<string> LogError = Console.Error.WriteLine;
        public ReflectionPolicy Policy = new ReflectionPolicy();

        public ReferenceFinder Initialize(Type targetType)
        {
            return Initialize(targetType, AppDomain.CurrentDomain.GetAssemblies());
        }
        public ReferenceFinder Initialize(Type targetType, IEnumerable<Assembly> assemblys)
        {
            return Initialize(targetType, assemblys.SelectMany(a => Policy.GetAssemblyTypes(a)));
        }
        public ReferenceFinder Initialize(Type targetType, IEnumerable<Type> types)
        {
            using (new ScopeTimer("Build type graph", LogInfo))
            {
                mGraph = new TypeGraph(targetType, types, Policy);
            }
            return this;
        }
        public void FindReferences(object targetObj, Action<string> callback)
        {
            FindReferences(targetObj, new[] { Assembly.GetCallingAssembly() }, callback);
        }
        public void FindReferences(object targetObj, IEnumerable<Assembly> assemblys, Action<string> callback)
        {
            var roots =
                from type in assemblys.SelectMany(a => Policy.GetAssemblyTypes(a))
                from root in
                    ((from field in Policy.GetStaticFields(type)
                      let path = string.Format("{0}.{1}", type.Name, field.Name)
                      let v = TryGetFieldValue(new object[] { path }, null, field)
                      where v != null
                      select new KeyValuePair<string, object>(path, v)))
                      .Concat(
                         from prop in Policy.GetStaticProperties(type)
                         let path = string.Format("{0}.{1}", type.Name, prop.Name)
                         let v = TryGetPropertyValue(new object[] { path }, null, prop)
                         where v != null
                         select new KeyValuePair<string, object>(path, v))
                select root;

            FindReferences(targetObj, roots, callback);
        }
        public void FindReferences(object targetObj, IEnumerable<KeyValuePair<string, object>> roots, Action<string> callback)
        {
            using (new ScopeTimer("Find all", LogInfo))
            {
                {
                    var path = new List<object>();
                    var findCache = new HashSet<object>();

                    foreach (var kv in roots)
                    {
                        using (new ScopeTimer(string.Format("Find in {0}", kv.Key), LogInfo))
                        {
                            path.Add(kv.Key);
                            FindReferencesRecursively(path, kv.Value, targetObj, findCache, callback);
                            path.RemoveAt(path.Count - 1);
                        }
                    }
                }

                using (new ScopeTimer("GC", LogInfo))
                {
                    GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced);
                }
            }
        }
        private void FindReferencesRecursively(List<object> path, object obj, object targetObj, HashSet<object> findCache, Action<string> callback)
        {
            if (obj == null) return;

            var type = obj.GetType();
            var targetType = targetObj.GetType();

            if (type == targetType && obj.Equals(targetObj))
            {
                callback(PathToString(path));
                return;
            }

            if (ReflectionPolicy.IsAtom(type)) return;

            var layout = mGraph.GetLayout(type);
            if (layout == null)
            {
                return;
            }

            if (findCache.Contains(obj)) return;
            findCache.Add(obj);

            if (type.IsArray)
            {
                var a = obj as Array;
                for (int i = 0, length = a.Length; i < length; ++i)
                {
                    path.Add(i);
                    FindReferencesRecursively(path, a.GetValue(i), targetObj, findCache, callback);
                    path.RemoveAt(path.Count - 1);
                }
            }
            else
            {
                foreach (var field in layout.Fields)
                {
                    path.Add(field.Name);
                    FindReferencesRecursively(path, TryGetFieldValue(path, obj, field), targetObj, findCache, callback);
                    path.RemoveAt(path.Count - 1);
                }
                foreach (var prop in layout.Properties)
                {
                    path.Add(prop.Name);
                    FindReferencesRecursively(path, TryGetPropertyValue(path, obj, prop), targetObj, findCache, callback);
                    path.RemoveAt(path.Count - 1);
                }
            }
        }
        private object TryGetFieldValue(IEnumerable<object> path, object o, FieldInfo field)
        {
            try
            {
                return field.GetValue(o);
            }
            catch (Exception e)
            {
                LogError(PathToString(path));
                LogError(e.ToString());
                return null;
            }
        }
        private object TryGetPropertyValue(IEnumerable<object> path, object o, PropertyInfo prop)
        {
            try
            {
                return prop.GetValue(o, null);
            }
            catch (Exception e)
            {
                LogError(PathToString(path));
                LogError(e.ToString());
                return null;
            }
        }
        private string PathToString(IEnumerable<object> path)
        {
            mCachedStrBuilder.Remove(0, mCachedStrBuilder.Length);

            var e = path.GetEnumerator();
            if (e.MoveNext()) mCachedStrBuilder.Append(e.Current);
            while (e.MoveNext())
            {
                mCachedStrBuilder.Append(".");
                mCachedStrBuilder.Append(e.Current.ToString());
            }
            return mCachedStrBuilder.ToString();
        }

        private TypeGraph mGraph;
        private StringBuilder mCachedStrBuilder = new StringBuilder(128);
    }
}

public class ReferenceFinderTestCase
{
    private class Unit
    {
        public string name;
    }

    private static ReferenceFinderTestCase Instance;

    private Unit mUnit;
    private List<Unit> mList { get; set; }
    private object mObj { get; set; }
    private Hashtable mHashtable { get; set; }
    private object mObjDict { get; set; }
    private object mObjTypedDict { get; set; }
    private Unit[] mArray { get; set; }
    private Action mAction { get; set; }
    private Func<string> mStrFunc { get; set; }
    private object[] mObjArray { get; set; }

    private static IEnumerable CreateEnumerable(Unit z)
    {
        yield return 1;
        yield return z;
    }
    private static IEnumerator<int> CreateEnumerator(Unit z)
    {
        yield return 1;
        Console.WriteLine(z);
        yield return 2;
    }
    private static void Setup()
    {
        Instance = new ReferenceFinderTestCase();

        var o = new Unit { name = "fads", };

        Instance.mUnit = o;
        Instance.mList = new List<Unit> { o, o };
        Instance.mObj = new { fieldZ = o };
        Instance.mHashtable = new Hashtable();
        Instance.mHashtable[o] = o;
        Instance.mObjDict = new Dictionary<object, object> { { o, o } };
        Instance.mObjTypedDict = new Dictionary<Unit, string> { { o, "fadfs" } };
        Instance.mArray = new Unit[] { o, o, o };

        Instance.mAction = () =>
        {
            Console.WriteLine(o);
        };
        Instance.mStrFunc = o.ToString;

        Instance.mObjArray = new object[] { o, new object[][] { new object[] { o, new List<object> { o } }, new object[] { (Func<Type>)o.GetType, (Func<int>)o.GetHashCode } }, CreateEnumerable(o), CreateEnumerator(o) };
    }
    private static void Cleanup()
    {
        Instance = null;
    }

    public static void Run()
    {
        Setup();

        using (var infoFile = File.CreateText("FindLog_Info.txt"))
        using (var errFile = File.CreateText("FindLog_Error.txt"))
        using (var outputFile = File.CreateText("FindLog_Output.txt"))
        {
            var obj = Instance.mUnit;

            var finder = new ReferenceAnalysisAlgorithm.ReferenceFinder()
            {
                LogInfo = infoFile.WriteLine,
                LogError = errFile.WriteLine,
            }.Initialize(obj.GetType());

            finder.FindReferences(obj, outputFile.WriteLine);
        }

        Cleanup();
    }
}

namespace CSharp13
{
    public class Program
    {
        public static void Main(string[] args)
        {
            ReferenceFinderTestCase.Run();
        }
    }
}
