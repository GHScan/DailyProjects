using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Linq;
using Newtonsoft.Json;

namespace Configuration
{
    [Serializable]
    public sealed class TimeAxis
    {
        public int Start { get; set; }
        public int End { get; set; }
        public int AnchorAlignment { get; set; }
    }

    [Serializable]
    public sealed class Event
    {
        public string Name { get; set; }
        public int Start { get; set; }
        public int End { get; set; }
    }

    [Serializable]
    public sealed class EventGroup
    {
        public string Name { get; set; }
        public bool Visible { get; set; }
        public List<Event> Events { get; set; }
    }

    [Serializable]
    public sealed class Scene
    {
        public float PixelsPerUnit { get; set; }
        public TimeAxis TimeAxis { get; set; }
        public List<EventGroup> EventGroups { get; set; }
    }
}

namespace Rendering
{
    public static class Style
    {
        public const string kFontName = "Monaco";
        public const int kFontSize = 8;

        public static Color BackgroundColor = Color.Gainsboro;
        public static Color FrameColor = Color.Black;
        public static Color FillColor = Color.Cornsilk;


        public const int kOuterBorder = 30;
        public const int kInnerBorder = 1;


        public const int kTimelineHeight = 4;


        public const int kEventHeight = 20;
        public const int kEventGroupGap = 10;
    }

    public sealed class RenderingContext : IDisposable
    {
        private Graphics mGraphics;
        private readonly Pen mFramePen;
        private readonly Pen mDashedFramePen;
        private readonly SolidBrush mFrameBrush;
        private readonly Font mFont;
        private readonly StringFormat mCenterAlignFormat;

        public RenderingContext(Graphics graphics)
        {
            mGraphics = graphics;
            mGraphics.Clear(Style.BackgroundColor);

            mFramePen = new Pen(Style.FrameColor);
            mDashedFramePen = new Pen(Style.FrameColor) { DashStyle = DashStyle.DashDot };
            mFrameBrush = new SolidBrush(Style.FrameColor);
            mFont = new Font(Style.kFontName, Style.kFontSize);
            mCenterAlignFormat = new StringFormat { LineAlignment = StringAlignment.Center };
        }

        public void Dispose()
        {
            if (mGraphics == null) return;
            mGraphics = null;

            mFramePen.Dispose();
            mDashedFramePen.Dispose();
            mFrameBrush.Dispose();
            mCenterAlignFormat.Dispose();
        }

        public void DrawLine(int x1, int y1, int x2, int y2)
        {
            mGraphics.DrawLine(mFramePen, x1, y1, x2, y2);
        }

        public void DrawDashedLine(int x1, int y1, int x2, int y2)
        {
            mGraphics.DrawLine(mDashedFramePen, x1, y1, x2, y2);
        }

        public void DrawRectangle(int x, int y, int width, int height)
        {
            mGraphics.DrawRectangle(mFramePen, x, y, width, height);
        }

        public void FillRectangle(int x, int y, int width, int height)
        {
            var rect = new Rectangle(x, y, width, height);

            using (var brush = new LinearGradientBrush(rect, Color.White, Style.FillColor, LinearGradientMode.Vertical))
                mGraphics.FillRectangle(brush, rect);
        }

        public void DrawString(string s, int x, int y)
        {
            mGraphics.DrawString(s, mFont, mFrameBrush, x, y);
        }

        public void DrawString(string s, int x, int y, int width, int height)
        {
            var font = ChooseFont(s, x, y, width, height);

            mGraphics.DrawString(s, font, mFrameBrush, new RectangleF(x, y, width, height), mCenterAlignFormat);

            if (!font.Equals(mFont)) font.Dispose();
        }

        private Font ChooseFont(string s, int x, int y, int width, int height)
        {
            var realSize = mGraphics.MeasureString(s, mFont, new PointF(x, y), mCenterAlignFormat);

            var scale = Math.Min(height / realSize.Height, Math.Min(width / realSize.Width, 1));
            if (scale == 1) return mFont;

            return new Font(mFont.FontFamily, mFont.Size * scale);
        }
    }

    public abstract class Renderable
    {
        public int X { get; set; }
        public int Y { get; set; }
        public int Width { get; set; }
        public int Height { get; set; }
        public abstract void Render(RenderingContext ctx);
    }

    public sealed class TimeAxis : Renderable
    {
        public int Start { get; set; }
        public int End { get; set; }
        public int AnchorAlignment { get; set; }

        public override void Render(RenderingContext ctx)
        {
            ctx.DrawLine(X, Y, X + Width, Y);
            ctx.DrawLine(X, Y + Height, X + Width, Y + Height);

            ctx.DrawDashedLine(X, Y, X, Y + Height);
            ctx.DrawDashedLine(X + Width, Y, X + Width, Y + Height);
            {
                var step = (double)AnchorAlignment / (End - Start);
                var first = (Math.Ceiling((double)Start / AnchorAlignment) * AnchorAlignment - Start) / (End - Start);
                for (var f = first; f < 0.9999; f += step)
                {
                    var x = X + (int)(f * Width);
                    ctx.DrawDashedLine(x, Y, x, Y + Height);
                }
            }

            ctx.DrawString(Start.ToString(), X, Y);
            ctx.DrawString(Start.ToString(), X, Y + Height);
            ctx.DrawString(End.ToString(), X + Width, Y);
            ctx.DrawString(End.ToString(), X + Width, Y + Height);
            {
                var step = (double)AnchorAlignment / (End - Start);
                var first = (Math.Ceiling((double)Start / AnchorAlignment) * AnchorAlignment - Start) / (End - Start);
                var v = (int)(Math.Ceiling((double)Start / AnchorAlignment) * AnchorAlignment);
                for (var f = first; f < 0.9999; f += step, v += AnchorAlignment)
                {
                    var x = X + (int)(f * Width);
                    ctx.DrawString(v.ToString(), x, Y);
                    ctx.DrawString(v.ToString(), x, Y + Height);
                }
            }
        }
    }

    public sealed class Event : Renderable
    {
        public string Name { get; set; }

        public override void Render(RenderingContext ctx)
        {
            ctx.DrawRectangle(X + Style.kInnerBorder, Y + Style.kInnerBorder, Width - 2 * Style.kInnerBorder, Height - 2 * Style.kInnerBorder);
            ctx.FillRectangle(X + 1 + Style.kInnerBorder, Y + 1 + Style.kInnerBorder, Width - 1 - 2 * Style.kInnerBorder, Height - 1 - 2 * Style.kInnerBorder);
            ctx.DrawString(Name, X + 1 + Style.kInnerBorder, Y + 1 + Style.kInnerBorder, Width - 1 - 2 * Style.kInnerBorder, Height - 1 - 2 * Style.kInnerBorder);
        }
    }

    public sealed class EventGroup : Renderable
    {
        public List<Event> Events { get; set; }
        public string Name { get; set; }

        public override void Render(RenderingContext ctx)
        {
            ctx.DrawString(Name, X, Y);

            foreach (var e in Events)
            {
                e.Render(ctx);
            }
        }
    }

    public sealed class Scene : Renderable
    {
        public TimeAxis TimeAxis { get; set; }
        public List<EventGroup> EventGroups { get; set; }

        public override void Render(RenderingContext ctx)
        {
            TimeAxis.Render(ctx);

            foreach (var group in EventGroups)
            {
                group.Render(ctx);
            }
        }
    }

    public sealed class Layouter
    {
        public static Scene Layout(Configuration.Scene config)
        {
            var scene = new Scene
            {
                X = 0,
                Y = 0,
                Width = (int)((config.TimeAxis.End - config.TimeAxis.Start) * config.PixelsPerUnit) + 2 * Style.kOuterBorder
            };

            scene.TimeAxis = new TimeAxis
            {
                X = Style.kOuterBorder,
                Y = Style.kOuterBorder,
                Width = scene.Width - 2 * Style.kOuterBorder,
                Start = config.TimeAxis.Start,
                End = config.TimeAxis.End,
                AnchorAlignment = config.TimeAxis.AnchorAlignment
            };


            var y = Style.kOuterBorder + Style.kTimelineHeight;

            scene.EventGroups = new List<EventGroup>();
            foreach (var group in config.EventGroups.Where(g => g.Visible))
            {
                y += Style.kEventGroupGap;

                scene.EventGroups.Add(Layout(
                    group,
                    Style.kOuterBorder,
                    y,
                    scene.Width - 2 * Style.kOuterBorder,
                    config.TimeAxis.Start, config.TimeAxis.End));

                y += scene.EventGroups.Last().Height;
            }

            scene.TimeAxis.Height = y + Style.kEventGroupGap + Style.kTimelineHeight - Style.kOuterBorder;

            scene.Height = scene.TimeAxis.Height + 2 * Style.kOuterBorder;

            return scene;
        }

        private static EventGroup Layout(
            Configuration.EventGroup config,
            int x, int y, int width, int start, int end)
        {
            var group = new EventGroup
            {
                X = x,
                Y = y,
                Width = width,
                Name = config.Name,
                Events = new List<Event>()
            };

            var overlappedEvents = new List<Configuration.Event>();
            foreach (var e in config
                .Events
                .Where(e => e.Start >= start && e.End <= end)
                .OrderBy(e => e.Start))
            {
                var depth = 0;
                for (; depth < overlappedEvents.Count && overlappedEvents[depth].End > e.Start; ++depth) ;
                if (depth == overlappedEvents.Count) overlappedEvents.Add(e);
                else overlappedEvents[depth] = e;

                group.Events.Add(new Event
                {
                    X = x + width * (e.Start - start) / (end - start),
                    Y = y + (1 + depth) * Style.kEventHeight,
                    Width = width * (e.End - e.Start) / (end - start),
                    Height = Style.kEventHeight,
                    Name = e.Name
                });
            }

            group.Height = (1 + overlappedEvents.Count) * Style.kEventHeight;

            return group;
        }
    }

    public sealed class ImageRender
    {
        public static void Render(string path, Scene scene)
        {
            using (var bmp = new Bitmap(scene.Width, scene.Height))
            using (var g = Graphics.FromImage(bmp))
            using (var ctx = new RenderingContext(g))
            {
                scene.Render(ctx);
                bmp.Save(path);
            }
        }
    }
}

namespace CSharp2015
{
    public class Program
    {
        public static int Main(string[] args)
        {
            if (args.Length < 1)
            {
                var cmdName = Path.GetFileName(System.Reflection.Assembly.GetEntryAssembly().Location);
                Console.Error.WriteLine("{0} render config.json output.png", cmdName);
                Console.Error.WriteLine("{0} genconfig config.json", cmdName);
                return 1;
            }

            switch (args[0].ToLower())
            {
                case "render":
                    {
                        var config = JsonConvert.DeserializeObject<Configuration.Scene>(File.ReadAllText(args[1]));
                        var scene = Rendering.Layouter.Layout(config);
                        Rendering.ImageRender.Render(args[2], scene);

                        return 0;
                    }
                case "genconfig":
                    {
                        var config = new Configuration.Scene
                        {
                            TimeAxis = new Configuration.TimeAxis(),
                            EventGroups = new List<Configuration.EventGroup>
                        {
                            new Configuration.EventGroup
                            {
                                Events = new List<Configuration.Event>
                                {
                                    new Configuration.Event()
                                }
                            }
                        },
                            PixelsPerUnit = 1
                        };

                        File.WriteAllText(args[1], JsonConvert.SerializeObject(config));

                        return 0;
                    }
                default:
                    throw new NotSupportedException("Unkown command :" + args[0]);
            }
        }
    }
}