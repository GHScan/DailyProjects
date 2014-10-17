using UnityEngine;
using UnityEditor;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

public class DepthCalculator : MonoBehaviour
{
    public List<UIWidget> Dependences;

    private void OnDrawGizmos()
    {
        if (Selection.activeTransform != transform) return;

        foreach (var w in Dependences)
        {
            var bounds = w.CalculateBounds(transform);
            Gizmos.matrix = transform.localToWorldMatrix;
            Gizmos.color = Color.green;
            Gizmos.DrawWireCube(bounds.center, bounds.size + new Vector3(1, 1, 0));
            Gizmos.DrawWireCube(bounds.center, bounds.size + new Vector3(2, 2, 0));
            Gizmos.DrawWireCube(bounds.center, bounds.size + new Vector3(3, 3, 0));
        }
    }

    [MenuItem("Tool/DepthCalculator/AdjustDepth(Bind+AdjustDepth+Unbind)")]
    private static void AdjustDepth()
    {
        var panel = Selection.activeTransform.GetComponent<UIPanel>();
        var widgets = GetPanelWidgets(panel, false);
        
        DepthCalculatorBindingAlgorithm.BindToWidgets(widgets);
        UnityEditor.Undo.RecordObjects(widgets, "AdjustDepth");
        DepthCalculationAlgorithm.CalculateDepthForWidgets(widgets);
        foreach (var w in widgets) 
        {
            var c = w.GetComponent<DepthCalculator>();
            if (c != null) NGUITools.Destroy(c);
        }
    }
    [MenuItem("Tool/DepthCalculator/Bind")]
    private static void Bind()
    {
        var panel = Selection.activeTransform.GetComponent<UIPanel>();
        DepthCalculatorBindingAlgorithm.BindToWidgets(GetPanelWidgets(panel, false));
    }

    [MenuItem("Tool/DepthCalculator/Unbind")]
    private static void Unbind()
    {
        foreach (var trans in Selection.transforms)
        {
            foreach (var c in trans.gameObject.GetComponentsInChildren<DepthCalculator>(true))
            {
                NGUITools.Destroy(c);
            }
        }
    }
    [MenuItem("Tool/DepthCalculator/CalculateDepth")]
    private static void CalculateDepth()
    {
        var panel = Selection.activeTransform.GetComponent<UIPanel>();
        var widgets = GetPanelWidgets(panel, false);

        UnityEditor.Undo.RecordObjects(widgets, "CalculateDepth");
        DepthCalculationAlgorithm.CalculateDepthForWidgets(widgets);
    }

    public static UIWidget[] GetPanelWidgets(UIPanel panel, bool includeInactive)
    {
        return panel.GetComponentsInChildren<UIWidget>(includeInactive)
            .Where(w => w.GetType() != typeof(UIWidget) && GetWidgetPanel(w) == panel)
            .ToArray();
    }
    public static UIPanel GetWidgetPanel(UIWidget w) 
    {
        return w.panel ?? UIPanel.Find(w.transform);
    }

    public static class DepthCalculatorBindingAlgorithm
    {
        public static void BindToWidgets(UIWidget[] widgets)
        {
            Array.Sort(widgets, UIWidget.PanelCompareFunc);

            for (var i = 1; i < widgets.Length; ++i)
            {
                var w = widgets[i];
                if (!w.gameObject.GetComponent<DepthCalculator>())
                {
                    var dependences = IntersectWidgetsWithBoundsList(widgets, 0, i, new[] { w.CalculateBounds(GetWidgetPanel(w).transform) });
                    if (dependences.Count > 0)
                    {
                        w.gameObject.AddComponent<DepthCalculator>().Dependences = dependences;
                    }
                }
            }
        }
        private static List<UIWidget> IntersectWidgetsWithBoundsList(UIWidget[] widgets, int off, int len, IEnumerable<Bounds> boundsList)
        {
            var result = new List<UIWidget>();

            for (var i = len - 1; i >= off; --i)
            {
                if (!boundsList.Any()) break;

                var widgetBounds = widgets[i].CalculateBounds(GetWidgetPanel(widgets[i]).transform);
                bool intersect = (from bounds in boundsList where bounds.Intersects(widgetBounds) select bounds).Any();
                if (intersect)
                {
                    result.Add(widgets[i]);
                    boundsList = from bounds in boundsList
                                 from fragBounds in BoundsSubstract(bounds, widgetBounds)
                                 where fragBounds.size.sqrMagnitude > 1.0f
                                 select fragBounds;
                    if (boundsList.Count() > 32)
                    {
                        boundsList = (from bounds in boundsList
                                      where bounds.size.sqrMagnitude > 4.0f
                                      orderby bounds.size.sqrMagnitude descending
                                      select bounds).Take(32);
                    }
                }
            }

            return result;
        }
        private static IEnumerable<Bounds> BoundsSubstract(Bounds a, Bounds b)
        {
            if (!a.Intersects(b))
            {
                yield return a;
            }

            if (a.min.x < b.min.x) yield return CreateBounds(new Vector3(a.min.x, a.min.y, a.center.z), new Vector3(b.min.x, a.max.y, a.center.z));
            if (a.max.x > b.max.x) yield return CreateBounds(new Vector3(b.max.x, a.min.y, a.center.z), new Vector3(a.max.x, a.max.y, a.center.z));
            if (a.min.y < b.min.y) yield return CreateBounds(new Vector3(Mathf.Max(a.min.x, b.min.x), a.min.y, a.center.z), new Vector3(Mathf.Min(a.max.x, b.max.x), b.min.y, a.center.z));
            if (a.max.y > b.max.y) yield return CreateBounds(new Vector3(Mathf.Max(a.min.x, b.min.x), b.max.y, a.center.z), new Vector3(Mathf.Min(a.max.x, b.max.x), a.max.y, a.center.z));
        }
        private static Bounds CreateBounds(Vector3 min, Vector3 max)
        {
            return new Bounds((min + max) * 0.5f, max - min);
        }
    }

    public static class DepthCalculationAlgorithm
    {
        class ComplexMaterial
        {
            private Material mMaterial;
            private Texture mTexture;
            private Shader mShader;
            public ComplexMaterial(Material material, Texture texture, Shader shader)
            {
                mMaterial = material;
                mTexture = texture;
                mShader = shader;
            }
            public override bool Equals(object obj)
            {
                var o = obj as ComplexMaterial;
                return mMaterial == o.mMaterial && mTexture == o.mTexture && mShader == o.mShader;
            }
            public override int GetHashCode()
            {
                return (mMaterial == null ? 1 : mMaterial.GetHashCode())
                    ^ (mTexture == null ? 2 : mTexture.GetHashCode())
                    ^ (mShader == null ? 3 : mShader.GetHashCode());
            }
            public override string ToString()
            {
                return mMaterial != null ? mMaterial.ToString() : base.ToString();
            }
            public static bool operator ==(ComplexMaterial a, ComplexMaterial b)
            {
                return System.Object.Equals(a, b);
            }
            public static bool operator !=(ComplexMaterial a, ComplexMaterial b)
            {
                return !System.Object.Equals(a, b);
            }
        }
        class WidgetNode
        {
            public UIWidget Widget;
            public ComplexMaterial Material;
            public int DependenceCount = 0;
            public List<WidgetNode> ReverseDependences = new List<WidgetNode>();
            public WidgetNode Prev = null, Next = null;
            public static bool Before(WidgetNode a, WidgetNode b)
            {
                if (a.DependenceCount == b.DependenceCount) return a.Widget.GetInstanceID() < b.Widget.GetInstanceID();
                return a.DependenceCount < b.DependenceCount;
            }
        }
        private static WidgetNode LinkWidgetNodeToList(WidgetNode list, WidgetNode node)
        {
            if (list == null || WidgetNode.Before(node, list))
            {
                if (list != null) list.Prev = node;
                node.Next = list;
                return node;
            }
            else
            {
                var n = list;
                for (; n.Next != null && WidgetNode.Before(n.Next, node); n = n.Next) ;
                node.Prev = n;
                node.Next = n.Next;
                if (n.Next != null) n.Next.Prev = node;
                n.Next = node;
                return list;
            }
        }
        private static WidgetNode UnlinkWidgetNodeFromList(WidgetNode list, WidgetNode node)
        {
            if (list == node) list = node.Next;

            if (node.Prev != null) node.Prev.Next = node.Next;
            if (node.Next != null) node.Next.Prev = node.Prev;
            node.Prev = node.Next = null;

            return list;
        }
        private static WidgetNode AdjustWidgetNodePosInList(WidgetNode list, WidgetNode node)
        {
            if ((node.Prev != null && WidgetNode.Before(node, node.Prev)) ||
                (node.Next != null && WidgetNode.Before(node.Next, node)))
            {
                list = UnlinkWidgetNodeFromList(list, node);
                list = LinkWidgetNodeToList(list, node);
            }
            return list;
        }
        private static List<WidgetNode> GetTopWidgetNodesOfList(WidgetNode list)
        {
            var result = new List<WidgetNode>();

            if (list != null)
            {
                var dependenceCount = list.DependenceCount;
                for (; list != null && list.DependenceCount == dependenceCount; list = list.Next) result.Add(list);
                result.Sort((a, b) => UIWidget.PanelCompareFunc(a.Widget, b.Widget));
            }

            return result;
        }
        private static void FindBestSolution(WidgetNode nodeList, ComplexMaterial lastMaterial, int drawCall, List<UIWidget> result, ref int bestDrawCall, List<UIWidget> bestResult, int stopDrawCall)
        {
            if (drawCall >= bestDrawCall) return;
            if (nodeList == null)
            {
                if (drawCall < bestDrawCall)
                {
                    bestDrawCall = drawCall;
                    bestResult.Clear();
                    bestResult.AddRange(result);
                }
                return;
            }

            var bestNodes = (from node in GetTopWidgetNodesOfList(nodeList)
                             where node.Material == lastMaterial
                             select node).Take(1);
            if (!bestNodes.Any())
            {
                bestNodes = from node in GetTopWidgetNodesOfList(nodeList)
                            group node by node.Material into g
                            select g.First();
            }

            foreach (var node in bestNodes.ToArray())
            {
                nodeList = UnlinkWidgetNodeFromList(nodeList, node);
                foreach (var reverseNode in node.ReverseDependences)
                {
                    --reverseNode.DependenceCount;
                    nodeList = AdjustWidgetNodePosInList(nodeList, reverseNode);
                }
                result.Add(node.Widget);

                FindBestSolution(nodeList, node.Material, lastMaterial == node.Material ? drawCall : drawCall + 1, result, ref bestDrawCall, bestResult, stopDrawCall);

                result.RemoveAt(result.Count - 1);
                foreach (var reverseNode in node.ReverseDependences)
                {
                    ++reverseNode.DependenceCount;
                    nodeList = AdjustWidgetNodePosInList(nodeList, reverseNode);
                }
                nodeList = LinkWidgetNodeToList(nodeList, node);

                if (bestDrawCall == stopDrawCall) break;
            }
        }
        public static void CalculateDepthForWidgets(UIWidget[] widgets)
        {
            var widget2Node = new Dictionary<UIWidget, WidgetNode>();
            WidgetNode nodeList = null;

            foreach (var w in widgets)
            {
                var node = new WidgetNode { Widget = w, Material = new ComplexMaterial(w.material, w.mainTexture, w.shader), };
                widget2Node[w] = node;
            }
            foreach (var node in widget2Node.Values)
            {
                var dependences = node.Widget.GetComponent<DepthCalculator>() ? node.Widget.GetComponent<DepthCalculator>().Dependences : new List<UIWidget>();
                node.DependenceCount = dependences.Count;
                foreach (var w in dependences)
                {
                    widget2Node[w].ReverseDependences.Add(node);
                }

                nodeList = LinkWidgetNodeToList(nodeList, node);
            }
            if (nodeList == null) return;

            var bestResult = new List<UIWidget>();
            int bestDrawCall = int.MaxValue;
            var stopDrawCall = widget2Node.Values.Select(n => n.Material).Distinct().Count();
            FindBestSolution(nodeList, null, 0, new List<UIWidget>(), ref bestDrawCall, bestResult, stopDrawCall);

            for (var i = 0; i < bestResult.Count; ++i) bestResult[i].depth = i;
        }
    }
}