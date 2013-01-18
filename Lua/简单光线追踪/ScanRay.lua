require "ScanTools"

Ray = {}

local methods = {}
methods.__index = methods
methods.__newindex = "can't modify class!"
methods.__getClassName = "Ray"

function Ray.new(ori, dir)
	if dir then
		local obj = {ori = ori, dir = dir}
		setmetatable(obj, methods)
		return obj
	else
		local obj = {}
		setmetatable(obj, getmetatable(ori))
		return obj
	end
end

function methods:getPoint(d)
	return self.ori + self.dir * d
end

function methods:intersect(obj)
	return obj:intersectRay(self)
end

function methods.__tostring(a)
	return tostring(a.ori) .. tostring(a.dir)
end

do
	IntersectResult = {}
	Ray.IntersectResult = IntersectResult

	function IntersectResult.new(mesh, dis, pos, nor, texX, texY)
		return {mesh = mesh, dis = dis, pos = pos, nor = nor, texX = texX, texY= texY}
	end
end
