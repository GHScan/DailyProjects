package
{	
	import flash.display.DisplayObject;
	import flash.display.DisplayObjectContainer;
	import flash.display.Loader;
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.filesystem.File;
	import flash.net.URLRequest;
	
	public class FlashBuilderTest extends Sprite
	{		
		public function FlashBuilderTest()
		{			
			var files: Array = new File('E:/图片/海贼王/七武海Q版').getDirectoryListing();
			for (var i : int = 0; i < 10; ++i)
			{
				var f : File = files[i] as File;
				if (f == null) break;
				if (f.nativePath.lastIndexOf('.gif') == -1) continue;
				
				var l : Loader = new Loader();
				l.name = i.toString();
				l.load(new URLRequest(f.nativePath));
				
				var dx : int = 0, dy : int = 0;
				var onEnterFrame : Function = function(e : Event) : void {
					var l : DisplayObject = e.target as DisplayObject;
					l.x = dx + l.parent.mouseX;
					l.y = dy + l.parent.mouseY;
				};
				l.addEventListener(MouseEvent.MOUSE_DOWN, function(e : MouseEvent) : void {
					var l : DisplayObject = e.target as DisplayObject;
					var parent : DisplayObjectContainer = l.parent;
					parent.removeChild(l);
					parent.addChild(l);
					l.addEventListener(Event.ENTER_FRAME, onEnterFrame);
					dx = l.x - l.parent.mouseX;
					dy = l.y - l.parent.mouseY;
				});
				var onMouseUp : Function = function(e : MouseEvent) : void {
					var l : DisplayObject = e.target as DisplayObject;
					l.removeEventListener(Event.ENTER_FRAME, onEnterFrame);
				}
				l.addEventListener(MouseEvent.MOUSE_UP, onMouseUp);
				l.x = int(i % 4) * 96;
				l.y = int(i / 4) * 128;
				this.addChild(l);
			}
		}
	}
}