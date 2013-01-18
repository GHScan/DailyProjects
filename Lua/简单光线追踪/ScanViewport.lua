require "gd"

Viewport = {}

local methods = {}
methods.__index = methods
methods.__newindex = "Can't modify class!"
methods.__getClassName = "Viewport"

function Viewport.new(w, h)
	local obj = {wDec = w - 1, hDec = h - 1, img = gd.createTrueColor(w, h)}
	setmetatable(obj, methods)
	return obj
end

function methods:createPixelIter()
	local x, y = 0, 0
	return
		function()
			if x > self.wDec then
				x, y = 0, y + 1
			end
			if y > self.hDec then
				return nil
			end
			local pt = {x = x, y = y}
			x = x + 1
			return pt
		end
end

function methods:getXY(pt)
	return (pt.x / self.wDec) * 2 - 1, (pt.y / self.hDec) * -2 + 1
end

function methods:setXYColor(x, y, color)
	color.x, color.y, color.z =
		math.max(math.min(1, color.x), 0),
		math.max(math.min(1, color.y), 0),
		math.max(math.min(1, color.z), 0)
	self.img:setPixel(
		(x + 1) / 2 * self.wDec,
		(y - 1) / -2 * self.hDec,
		self.img:colorAllocate(color.x * 255, color.y *255, color.z * 255))
end

function methods:flushToPng(name)
	self.img:png(name)
end
