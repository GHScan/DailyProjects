require "ScanRay"
require "ScanTools"

DefaultMesh = {}

do
	local Sphere = {}
	DefaultMesh.Sphere = Sphere

	local methods = {}
	methods.__index = methods
	methods.__newindex = "can't modify class!"
	methods.__getClassName = "DefaultMesh.Sphere"

	function Sphere.new(center, radius)
		local obj = {center = center, radius = radius}
		obj.sqrRadius = obj.radius * obj.radius
		setmetatable(obj, methods)
		return obj
	end

	function methods:applyPosOri(pos, ori)
		self.center = self.center + pos
	end

	function methods:intersectRay(r)
		local a = self.center - r.ori
		local b = a:dot(r.dir) * r.dir
		local c = a - b
		local cSqrLen = c:getSqrLength()
		if self.sqrRadius >= cSqrLen then
			d = math.sqrt(self.sqrRadius - cSqrLen)
			bDis = b.x / r.dir.x
			if bDis >= d then
				d = bDis - d
			else
				d = bDis + d
			end
			if d >= 0 then
				local pt = r:getPoint(d)
				return Ray.IntersectResult.new(
					self, d, pt, (pt - self.center):normalize())
			end
		end
	end

end

do
	Plane = {}
	DefaultMesh.Plane = Plane

	local methods = {}
	methods.__index = methods
	methods.__newindex = "Can't modify class!"
	methods.__getClassName = "Plane"

	function Plane.new(pos, nor, right, w, h)
		local obj = {
			pos = pos, nor = nor, right = right,
			halfW = w * 0.5, halfH = h * 0.5 }
		obj.up = obj.right:cross(obj.nor)
		setmetatable(obj, methods)
		return obj
	end

	function methods:applyPosOri(pos, ori)
		self.nor = ori:transform(self.nor)
		self.right = ori:transform(self.right)
		self.up = ori:transform(self.up)
		self.pos = self.pos + pos
	end

	function methods:intersectRay(r)
		local nDotDir = r.dir:dot(self.nor)
		if Tools.isFloatEqual(nDotDir, 0) then
		else
			--one intersect
			local h = (self.pos - r.ori):dot(self.nor)
			local dis = h / nDotDir
			if dis >= 0 then
				local pt = r:getPoint(dis)
				local toPt = pt - self.pos
				local x = toPt:dot(self.right)
				local y = toPt:dot(self.up)
				if x >= -self.halfW and x <= self.halfW and
					y >= -self.halfH and y <= self.halfH then
					return
					Ray.IntersectResult.new(
					self, dis, pt, self.nor,
					(x / self.halfW + 1) / 2,
					(y / self.halfH + 1) / 2)
				end
			end
		end
	end
end
