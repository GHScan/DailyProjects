using System;

using System.Linq;
using System.Drawing;
using System.Drawing.Imaging;
using System.Diagnostics;

namespace Graphics2D
{
    static class HelpFuncs
    {
        public static float SmoothStep(float min, float max, float val)
        {
            Debug.Assert(min <= max);
            if (val < min) return 0;
            else if (val > max) return 1;
            else return (val - min) / (max - min);
        }

        public static float Lerp(float min, float max, float c)
        {
            Debug.Assert(c >= 0 && c <= 1);
            return (max - min) * c + min;
        }

        public static void Swap<T>(ref T a, ref T b)
        {
            T t = a;
            a = b;
            b = t;
        }
    }

    struct LineSegment
    {
        public LineSegment(Point start, Point end)
        {
            m_start = start;
            m_end = end;
        }

        public int intersectWithY(int y, out int x)
        {
            int dyStart = m_start.Y - y;
            int dyEnd = m_end.Y - y;
            if (dyStart == 0 && dyEnd == 0)
            {
                x = 0;
                return 2; // 无穷个交点
            }
            else if (dyStart * dyEnd <= 0)
            {
                if (m_start.Y <= m_end.Y)
                {
                    float f = HelpFuncs.SmoothStep(m_start.Y, m_end.Y, y);
                    x = (int)HelpFuncs.Lerp(m_start.X, m_end.X, f);
                }
                else
                {
                    float f = HelpFuncs.SmoothStep(m_end.Y, m_start.Y, y);
                    x = (int)HelpFuncs.Lerp(m_end.X, m_start.X, f);
                }
                return 1; // 1个焦点
            }
            else
            {
                x = 0;
                Debug.Assert(dyStart * dyEnd > 0);
                return 0;
            }
        }

        public Point Start { get { return m_start; } }
        public Point End { get { return m_end; } }

        private Point m_start, m_end;
    }

    public struct Vec2f
    {
        public Vec2f(float n):this() { X = n; Y = n; }
        public Vec2f(float x, float y): this() { X = x; Y = y; }

        public float X { get; set; }
        public float Y { get; set; }

        public static explicit operator Point(Vec2f v)
        {
            return new Point((int)v.X, (int)v.Y);
        }

        public static implicit operator Vec2f(Point pt)
        {
            return new Vec2f(pt.X, pt.Y);
        }
    }

    public class Triangle
    {
        public Triangle(Point pt0, Point pt1, Point pt2)
        {
            m_pts = new Vec2f[]
            {
                pt0, pt1, pt2,
            };
        }

        public void Draw(Bitmap bm, BitmapData bmData, Color c)
        {
            LineSegment[] lineSegs = 
            {
                new LineSegment((Point)m_pts[0], (Point)m_pts[1]),
                new LineSegment((Point)m_pts[1], (Point)m_pts[2]),
                new LineSegment((Point)m_pts[2], (Point)m_pts[0]),
            };

            int minY = Math.Max(Math.Min(Math.Min((int)m_pts[0].Y, (int)m_pts[1].Y), (int)m_pts[2].Y), 0);
            int maxY = Math.Min(Math.Max(Math.Max((int)m_pts[0].Y, (int)m_pts[1].Y), (int)m_pts[2].Y), bm.Height - 1);

            for (int y = minY; y <= maxY; ++y)
            {
                int xCnt = 0;
                int x1 = 0, x2 = 0;
                for (int i = 0; i < lineSegs.Length; ++i)
                {
                    int tx = 0, tIntersectCnt = 0;
                    tIntersectCnt = lineSegs[i].intersectWithY(y, out tx);
                    
                    if (tIntersectCnt == 0) continue;
                    else if (tIntersectCnt == 2) { x1 = lineSegs[i].Start.X; x2 = lineSegs[i].End.X; xCnt = 2; break; }
                    else
                    {
                        if (xCnt == 0) {x1 = tx; xCnt = 1;}
                        else if (xCnt == 1) { x2 = tx; xCnt = 2; }
                        else 
                        {
                            if (x1 == x2) x2 = tx;
                            break;
                        }
                    }
                }

                if (xCnt == 0) continue;
                Debug.Assert(xCnt == 2);

                if (x1 > x2) { HelpFuncs.Swap(ref x1, ref x2); }
                x1 = Math.Max(x1, 0);
                x2 = Math.Min(x2, bm.Width - 1);

                unsafe
                {
                    int* p = (int*)bmData.Scan0 + y * bmData.Stride / 4;
                    int argb = c.ToArgb();
                    for (int x = x1; x <= x2; ++x)
                    {
                        p[x] = argb;
                    }
                }
            }
        }

        public void Rotate(float degree)
        {
            float angle = degree * 3.1415926f / 180.0f;

            Vec2f center = GetCenterPoint();

            for (int i = 0; i < m_pts.Length; ++i)
            {
                Vec2f pt = m_pts[i];
                pt.X -= center.X; pt.Y -= center.Y;
                float newX = (float)(pt.X * Math.Cos(angle) - pt.Y * Math.Sin(angle));
                float newY = (float)(pt.X * Math.Sin(angle) + pt.Y * Math.Cos(angle));
                pt.X = newX + center.X;
                pt.Y = newY + center.Y;
                m_pts[i] = pt;
            }
        }

        public void Translate(int x, int y)
        {
            for (int i = 0; i < m_pts.Length; ++i)
            {
                Vec2f pt = m_pts[i];
                pt.X += x; pt.Y += y;
                m_pts[i] = pt;
            }
        }

        public void Scale(float scale)
        {
            Vec2f center = GetCenterPoint();

            for (int i = 0; i < m_pts.Length; ++i)
            {
                Vec2f pt = m_pts[i];
                pt.X -= center.X; pt.Y -= center.Y;

                pt.X = scale * pt.X;
                pt.Y = scale * pt.Y;

                pt.X += center.X; pt.Y += center.Y;
                m_pts[i] = pt;
            }
        }

        public Vec2f GetCenterPoint()
        {
            return new Vec2f(
                (m_pts[0].X + m_pts[1].X + m_pts[2].X) / 3,
                (m_pts[0].Y + m_pts[1].Y + m_pts[2].Y) / 3);
        }

        private Vec2f[] m_pts;
    }
}