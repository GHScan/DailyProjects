require "ScanCamera"
require "ScanDefaultMesh"
require "ScanViewport"
require "ScanDefaultMaterial"
require "ScanLight"
require "ScanCamera"


local c = Camera.new(1, 60, 1.33)
c:applyPosOri(Vector3.new(0, 1, 0), Quaternion.fromAxisAngle(Vector3.UNIT_X, -10))

local l = Light.Point.new(
	Vector3.new(3, 2, 0), 10, Vector3.new(1, 0, 0),
	Vector3.new(0.8, 0.8, 0.8), Vector3.new(0.9, 0.9, 0.9), Vector3.new(0.3, 0.3, 0.3))

local units = {}

local tmp = {
	geometry = DefaultMesh.Sphere.new(Vector3.new(1.5, 0.9, 4), 1),
	material = DefaultMaterial.SolidColor.new(
	Vector3.new(0.3, 0.3, 0.3), Vector3.new(1, 0, 0), Vector3.new(0.9, 0.9, 0.9), 4)}
table.insert(units, tmp)

tmp = {
	geometry = DefaultMesh.Sphere.new(Vector3.new(-0.1, 0.9, 3), 1),
	material = DefaultMaterial.SolidColor.new(
	Vector3.new(0.3, 0.3, 0.3), Vector3.new(0, 1, 0), Vector3.new(0.9, 0.9, 0.9), 4)}
table.insert(units, tmp)

tmp = {
	geometry = DefaultMesh.Sphere.new(Vector3.new(-2, 0.9, 3), 1),
	material = DefaultMaterial.SolidColor.new(
	Vector3.new(0.3, 0.3, 0.3), Vector3.new(0, 0, 1), Vector3.new(0.9, 0.9, 0.9), 4)}
table.insert(units, tmp)

tmp = {
	geometry = DefaultMesh.Plane.new(Vector3.new(0, 0, 1), -Vector3.UNIT_Z, Vector3.UNIT_X, 10, 10),
	material = DefaultMaterial.ChessBoard.new(0.0625)}
tmp.geometry:applyPosOri(Vector3.new(0, 0, 0), Quaternion.fromAxisAngle(Vector3.UNIT_X, 90))
table.insert(units, tmp)


local w, h = 2048, 1536
--local w, h = 1024, 768
--local w, h = 128, 96

local vp = Viewport.new(w, h)

for pt in vp:createPixelIter() do

	local x, y = vp:getXY(pt)
	local r = c:getRay(x, y)

	local color = Vector3.ZERO
	local factor, reflectFac = 1, 0.7

	for i = 0, 4 do

		local result = false
		local m = false

		for i, v in ipairs(units) do
			local _r = r:intersect(v.geometry)
			if _r and (not result or (result and _r.dis < result.dis)) then
				result = _r
				m = v.material
			end
		end

		if result then
			local clr =
				l:calcColor(
					result.pos, result.nor, c.pos,
					m:getColor(result.texX, result.texY))
			local f = factor * reflectFac
			factor = factor * (1 - reflectFac)
			color = color + f * clr

			r = Ray.new(result.pos, Vector3.reflect(r.dir, result.nor))
			r.ori = r:getPoint(0.001)
		else
			break
		end
	end

	vp:setXYColor(x, y, color)

end

vp:flushToPng("1.png")

print("ºÄÊ±", os.clock())
require "ScanVector3"
require "ScanQuaternion"
require "ScanMatrix3"
require "ScanTools"
require "ScanRay"

Camera = {}

local methods = {}
methods.__index = methods
methods.__newindex = "can't modify class"
methods.__getClassName = "Camera"

function Camera.new(d, fovY, aspect)
	local obj = {d = d, fovY = fovY, aspect = aspect}
	obj.screenCenter = false
	obj.eyeToCenter = false
	obj.right = false
	obj.up = false
	obj.pos = false
	setmetatable(obj, methods)

	obj:applyPosOri(Vector3.ZERO, Quaternion.IDENTITY)

	return obj
end

function methods:getRay(rx, ry)
	local rightUp = rx * self.right + ry * self.up
	local dir = rightUp + self.eyeToCenter
	return Ray.new(self.screenCenter + rightUp, dir:normalize())
end

function methods:applyPosOri(pos, ori)
	local mat = Matrix3.fromAxisAngle(ori:toAxisAngle())
	self.eyeToCenter = mat.z * self.d
	self.up = mat.y * math.tan(Tools.degreeToRadian(self.fovY * 0.5)) * self.eyeToCenter:getLength()
	self.right = mat.x * self.up:getLength() * self.aspect
	self.screenCenter = pos + self.eyeToCenter
	self.pos = pos
end
require "ScanVector3"

DefaultMaterial = {}

do
	SolidColor = {}
	DefaultMaterial.SolidColor = SolidColor

	local methods = {}
	methods.__index = methods
	methods.__newindex = "can't modify class"
	methods.__getClassName = "DefualtMaterial.SolidColor"

	function SolidColor.new(amb, diff, spec, pow)
		local obj = {amb = amb, diff = diff, spec = spec, pow = pow}
		setmetatable(obj, methods)
		return obj
	end

	function methods:getColor(x, y)
		return self.amb, self.diff, self.spec, self.pow
	end

end

do
	ChessBoard = {}
	DefaultMaterial.ChessBoard = ChessBoard

	local methods = {}
	methods.__index = methods
	methods.__newindex = "can't modify class"
	methods.__getClassName = "DefualtMaterial.ChessBoard"

	function ChessBoard.new(
		gridSize, amb1, diff1, spec1, pow1, amb2, diff2, spec2, pow2)
		amb1 = amb1 or Vector3.ZERO
		diff1 = diff1 or Vector3.new(1, 1, 1)
		spec1 = spec1 or diff1 * 0.3
		pow1 = pow1 or 5
		amb2 = amb2 or amb1
		diff2 = diff2 or Vector3.ZERO
		spec2 = spec2 or spec1
		pow2 = pow2 or pow1

		local obj = {
			amb1 = amb1, diff1 = diff1, spec1 = spec1, pow1 = pow1,
			amb2 = amb2, diff2 = diff2, spec2 = spec2, pow2 = pow2,
			gridSize = gridSize}
		setmetatable(obj, methods)
		return obj
	end

	function methods:getColor(x, y)
		local sel =
			(math.floor(x / self.gridSize) + math.floor(y / self.gridSize)) % 2
		if sel == 0 then
			return self.amb1, self.diff1, self.spec1, self.pow1
		else
			return self.amb2, self.diff2, self.spec2, self.pow2
		end
	end

end
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
require "ScanVector3"

Light = {}

local function calcAtten(dis, atten)
	return 1 / (atten.x + atten.y * dis + atten.z * dis * dis)
end

do
	local Direction = {}
	Light.Direction = Direction

	local methods = {}
	methods.__index = methods
	methods.__newindex = "Can't modify class!"
	methods.__getClassName = "Light.Direction"

	function Direction.new(dir, diff, spec, amb)
		local obj = {dir = dir, diff = diff, spec = spec, amb = amb}
		setmetatable(obj, methods)
		return obj
	end

	function methods:calcColor(pos, nor, eyePos, amb, diff, spec, pow)
		local clr = amb * self.amb
		local toLight = -self.dir
		local nDotL = nor:dot(toLight)
		if nDotL > 0 and diff then
			clr = clr + diff * self.diff * nDotL
			if spec then
				local toEye = (eyePos - pos):normalize()
				local h = (toEye + toLight):normalize()
				local nDotH = nor:dot(h)
				if nDotH > 0 then
					clr = clr + spec * self.spec * math.pow(nDotH, pow)
				end
			end
		end
		return clr
	end

end

do
	local Point = {}
	Light.Point = Point

	local methods = {}
	methods.__index = methods
	methods.__newindex = "Can't modify class!"
	methods.__getClassName = "Light.Point"

	function Point.new(pos, range, atten, diff, spec, amb)
		local obj = {pos = pos, range = range, atten = atten, diff = diff, spec = spec, amb = amb}
		setmetatable(obj, methods)
		return obj
	end

	function methods:calcColor(pos, nor, eyePos, amb, diff, spec, pow)
		local toLight, dis = (self.pos - pos):normalize()
		if dis > self.range then
			return Vector3.ZERO
		end
		local atten = calcAtten(dis, self.atten)
		local nDotL = toLight:dot(nor)
		local clr = amb * self.amb
		if nDotL > 0 and diff then
			clr = clr + diff * self.diff * nDotL
			if spec then
				local input = (pos - eyePos):normalize()
				local output = Vector3.reflect(input, nor)
				local lDotO = toLight:dot(output)
				if lDotO > 0 then
					clr = clr + spec * self.spec * math.pow(lDotO, pow)
				end
			end
		end
		return clr * atten
	end

end

do
	local Spot = {}
	Light.Spot = Spot

end
require "ScanVector3"
require "ScanTools"

Matrix3 = {}

local methods = {}
methods.__index =
	function (obj, func)
		if type(func) == "number" then
			if func == 0 then
				return obj.x
			elseif func == 1 then
				return obj.y
			elseif func == 2 then
				return obj.z
			else
				assert(nil, "Out of range!")
			end
		else
			return methods[func]
		end
	end
methods.__newindex = "can't modify class!"
methods.__getClassName = "Matrix3"

function Matrix3.new(x, y, z)
	if y and z then
		local obj = {x = x, y = y, z = z}
		setmetatable(obj, methods)
		return obj
	else
		local obj = {x = x.x, y = x.y, z = x.z}
		setmetatable(obj, getmetatable(x))
		return obj
	end
end

function Matrix3.fromAxisAngle(axis, degree)
	local radian = Tools.degreeToRadian(degree)
	local cosR, sinR = math.cos(radian), math.sin(radian)
	local oneSubCos = 1 - cosR
	local axis_2 = axis * axis
	local xyzm = Vector3.new(
		axis.x * axis.y * oneSubCos,
		axis.x * axis.z * oneSubCos,
		axis.y * axis.z * oneSubCos)
	local axisSin = axis * sinR

	return
		Matrix3.new(
		Vector3.new(axis_2.x * oneSubCos + cosR, xyzm.x - axisSin.z, xyzm.y + axisSin.y),
		Vector3.new(xyzm.x + axisSin.z, axis_2.y * oneSubCos + cosR, xyzm.z - axisSin.x),
		Vector3.new(xyzm.y - axisSin.y, xyzm.z + axisSin.x, axis_2.z * oneSubCos + cosR))
end

Matrix3.IDENTITY = Matrix3.new(Vector3.UNIT_X, Vector3.UNIT_Y, Vector3.UNIT_Z)

function methods:toAxisAngle()

	local trace = self.x.x + self.y.y + self.z.z
	local cosTrace = 0.5 * (trace - 1)
	local radian = math.acos(cosTrace)
	local degree = Tools.radianToDegree(radian)

	if degree > 0 then
		if degree < 180 then
			local axis = Vector3.new(
				self.z.y - self.y.z,
				self.x.z - self.z.x,
				self.y.x - self.x.y)
			return axis:normalize(), degree
		else
			if self.x.x >= self.y.y then
				if self.x.x >= self.z.z then
					local x = 0.5 * math.sqrt(self.x.x - self.y.y - self.z.z + 1)
					local halfInvX = 0.5 / x
					return Vector3.new(x, halfInvX * self.x.y, halfInvX * self.x.z)
				else
					local z = 0.5 * math.sqrt(self.z.z - self.x.x - self.y.y + 1)
					local halfInvZ = 0.5 / z
					return Vector3.new(halfInvZ * self.x.z, halfInvZ * self.y.z, z)
				end
			else
				if self.y.y >= self.z.z then
					local y = 0.5 * math.sqrt(self.y.y - self.x.x - self.z.z + 1)
					local halfInvY = 0.5 / y
					return Vector3.new(halfInvY * self.x.y, y, halfInvY * self.y.z)
				else
					local z = 0.5 * math.sqrt(self.z.z - self.x.x - self.y.y + 1)
					local halfInvZ = 0.5 / z
					return Vector3.new(halfInvZ * self.x.z, halfInvZ * self.y.z, z)
				end
			end
		end
	else
		return Vector3.UNIT_X, degree
	end

end

function methods:transpose()
	return Matrix3.new(
		Vector3.new(self.x.x, self.y.x, self.z.x),
		Vector3.new(self.x.y, self.y.y, self.z.y),
		Vector3.new(self.x.z, self.y.z, self.z.z))
end

function methods:inverse()
	local mat =  Matrix3.new(
		Vector3.new(
			self[1][1] * self[2][2] - self[1][2] * self[2][1],
			self[0][2] * self[2][1] - self[0][1] * self[2][2],
			self[0][1] * self[1][2] - self[0][2] * self[1][1]),
		Vector3.new(
			self[1][2] * self[2][0] - self[1][0] * self[2][2],
			self[0][0] * self[2][2] - self[0][2] * self[2][0],
			self[0][2] * self[1][0] - self[0][0] * self[1][2]),
		Vector3.new(
			self[1][0] * self[2][1] - self[1][1] * self[2][0],
			self[0][1] * self[2][0] - self[0][0] * self[2][1],
			self[0][0] * self[1][1] - self[0][1] * self[1][0]))

	local def = self.x:dot(Vector3.new(mat[0][0], mat[1][0], mat[2][0]))

	if def > 0.00001 then
		def = 1 / def

		for i = 0, 2 do
			mat[i][0], mat[i][1], mat[i][2] = mat[i][0] * def, mat[i][1] * def, mat[i][2] * def
		end
	end

	return mat
end

function methods:transform(pos)
	return Vector3.new(
	pos.x * self.x.x + pos.y * self.y.x + pos.z * self.z.x,
	pos.x * self.x.y + pos.y * self.y.y + pos.z * self.z.y,
	pos.x * self.x.z + pos.y * self.y.z + pos.z * self.z.z)
end

function methods.__add(a, b)
	return Matrix3.new(
		a.x + b.x, a.y + b.y, a.z + b.z)
end

function methods.__sub(a, b)
	return Matrix3.new(
		a.x - b.x, a.y - b.y, a.z - b.z)
end

function methods.__mul(a, b)
	b = b:transpose()
	return Matrix3.new(
		Vector3.new(a.x:dot(b.x), a.x:dot(b.y), a.x:dot(b.z)),
		Vector3.new(a.y:dot(b.x), a.y:dot(b.y), a.y:dot(b.z)),
		Vector3.new(a.z:dot(b.x), a.z:dot(b.y), a.z:dot(b.z)))
end

function methods.__unm(a)
	return Matrix.new(-a.x, -a.y, -a.z)
end

function methods.__eq(a, b)
	return a.x == b.x and a.y == b.y and a.z == b.z
end

function methods.__tostring(a)
	return tostring(a.x) .. tostring(a.y) .. tostring(a.z)
end
OpFunc = {}

function OpFunc.add(a, b)
	return a + b
end

function OpFunc.sub(a, b)
	return a - b
end

function OpFunc.mul(a, b)
	return a * b
end

function OpFunc.div(a, b)
	return a / b
end
require "ScanTools"
require "ScanVector3"

Quaternion = {}

local methods = {}
methods.__index = methods
methods.__newindex = "can't modify class"
methods.__getClassName = "Quaternion"

function Quaternion.new(w, x, y, z)
	if x == nil then
		local obj = {v = w.v, w = w.w}
		setmetatable(obj, getmetatable(w))
		return obj
	else
		local obj = {v = Vector3.new(x, y, z), w = w}
		setmetatable(obj, methods)
		return obj
	end
end

function Quaternion.fromAxisAngle(axis, degree)
	local halfRadian = Tools.degreeToRadian(degree) * 0.5
	local sinR, cosR = math.sin(halfRadian), math.cos(halfRadian)
	return Quaternion.new(cosR, axis.x * sinR, axis.y * sinR, axis.z * sinR)
end

Quaternion.IDENTITY = Quaternion.new(1, 0, 0, 0)
Quaternion.ZERO = Quaternion.new(0, 0, 0, 0)

function methods:toAxisAngle()
	local sqrLen = self.v:getSqrLength()
	if sqrLen > 0 then
		local radian = math.acos(self.w) * 2
		local invSqrt = 1 / math.sqrt(sqrLen)
		return self.v * invSqrt, Tools.radianToDegree(radian)
	else
		return Vector3.UNIT_X, 0
	end
end

function methods:transform(v)
	local uv = self.v:cross(v)
	local uuv = self.v:cross(uv)
	uv = 2 * self.w * uv
	uuv = 2 * uuv
	return v + uv + uuv
end

function methods:inverse()
	local norm = self.v:getSqrLength() + self.w * self.w
	if norm > 0 then
		local invNorm = 1 / norm
		return Quaternion.new(
			self.w * invNorm,
			self.v.x * -invNorm,
			self.v.y * -invNorm,
			self.v.z * -invNorm)
	else
		return Quaternion.ZERO
	end
end

function methods:dot(o)
	return self.w * o.w + self.v:dot(o.v)
end

function methods.__mul(a, b)
	return
		Quaternion.new(
		a.w * b.w - a.v.x * b.v.x - a.v.y * b.v.y - a.v.z * b.v.z,
		a.w * b.v.x + a.v.x * b.w + a.v.y * b.v.z - a.v.z * b.v.y,
		a.w * b.v.y + a.v.y * b.w + a.v.z * b.v.x - a.v.x * b.v.z,
		a.w * b.v.z + a.v.z * b.w + a.v.x * b.v.y - a.v.y * b.v.x)
end

function methods.__unm(a)
	return Quaternion.new(-a.w, -a.v.x, -a.v.y, -a.v.z)
end

function methods.__eq(a, b)
	local radian = math.acos(a:dot(b))
	local degree = Tools.radianToDegree(radian)
	return Tools.isFloatEqual(degree, 0) or Tools.isFloatEqual(degree, 180)
end

function methods.__tostring(a)
	return "[w=" .. tostring(a.w) ..
			",x=" .. tostring(a.v.x) ..
			",y=" .. tostring(a.v.y) ..
			",z=" .. tostring(a.v.z) .. "]"
end
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
require "ScanVector3"
require "ScanQuaternion"

SpaceNode = {}

local methods = {}
methods.__index = methods
methods.__newindex = "can't modify class!"
methods.__getClassName = "SpaceNode"

function SpaceNode.new(pos, ori)
	pos = pos or Vector3.ZERO
	ori = ori or Quaternion.IDENTITY

	local obj = {pos = pos, ori = ori, meshs = {}}
	setmetatable(obj, methods)
	return obj
end

function methods:move(relPos)
	if type(relPos) == "number" then
		relPos = Vector3.new(0, 0, relPos)
	end
	relPos = self.ori:transform(relPos)
	self.pos = self.pos + relPos
end

function methods:pitch(degree)
	return self:rotate(Vector3.UNIT_X, degree)
end

function methods:yaw(degree)
	return self:rotate(Vector3.UNIT_Y, degree)
end

function methods:roll(degree)
	return self:rotate(Vector3.UNIT_Z, degree)
end

function methods:rotate(axis, degree)
	self.ori = self.ori * Quaternion.fromAxisAngle(axis, degree)
end

function methods:attachMesh(obj)
	self.meshs[obj] = true
end

function methods:detachMesh(obj)
	self.meshs[obj] = nil
end

function methods:iterateMeshs(func)
	for k, v in pairs(self.meshs) do
		if v == true then
			func(k, self.pos, self.ori)
		end
	end
end
Tools = {}

function Tools.getClassName(obj)
	typeObj = type(obj)
	if typeObj == "table" then
		return obj.__getClassName
	end
	return typeObj
end

local _PI_180 = 3.1415926 / 180
local _180_PI = 1 / _PI_180

function Tools.degreeToRadian(d)
	return d * _PI_180
end

function Tools.radianToDegree(r)
	return r * _180_PI
end

function Tools.isFloatEqual(a, b, epsilon)
	epsilon = epsilon or 0.00001
	return math.abs(a - b) < epsilon
end
require "ScanOpFunc"
require "ScanTools"

Vector3 = {}

local methods = {}
methods.__index =
	function (obj, func)
		if type(func) == "number" then
			if func == 0 then
				return obj.x
			elseif func == 1 then
				return obj.y
			elseif func == 2 then
				return obj.z
			else
				assert(nil, "Out of range!")
			end
		else
			return methods[func]
		end
	end
methods.__newindex =
	function(obj, func, v)
		if type(func) == "number" then
			if func == 0 then
				obj.x = v
			elseif func == 1 then
				obj.y = v
			elseif func == 2 then
				obj.z = v
			else
				assert(nil, "Out of range!")
			end
		else
			error("Can't modify class!")
		end
	end
methods.__getClassName = "Vector3"

function Vector3.new(x, y, z)
	if type(x) == "table" then
		local obj = {x = x.x, y = x.y, z = x.z}
		setmetatable(obj, getmetatable(src))
		return obj
	else
		if y == nil and z == nil then
			y, z = x, x
		end
		local obj = { x = x, y = y, z = z}
		setmetatable(obj, methods)
		return obj
	end
end

function Vector3.reflect(i, nor)
	return i - i:dot(nor) * nor * 2
end

function Vector3.refract(i, nor, fac)

end

Vector3.UNIT_X = Vector3.new(1, 0, 0)
Vector3.UNIT_Y = Vector3.new(0, 1, 0)
Vector3.UNIT_Z = Vector3.new(0, 0, 1)
Vector3.ZERO = Vector3.new(0, 0, 0)

function methods:getSqrLength()
	return self:dot(self)
end

function methods:getLength()
	return math.sqrt(self:getSqrLength())
end

function methods:normalize()
	local l = self:getLength()
	if l > 0.00001 then
		return self / l, l
	end
	return nil, l
end

function methods:dot(o)
	return self.x * o.x + self.y * o.y + self.z * o.z
end

function methods:cross(o)
	return Vector3.new(
	self.y * o.z - o.y * self.z,
	self.z * o.x - o.z * self.x,
	self.x * o.y - o.x * self.y
	)
end

local function opAB(a, b, op)
	local typeA = type(a)
	local typeB = type(b)
	if typeA == "table" and typeB == "table" then
	--both vec3
		return Vector3.new(op(a.x, b.x), op(a.y, b.y), op(a.z, b.z))
	elseif typeA == "table" then
	--a is vec3
		return Vector3.new(op(a.x, b), op(a.y, b), op(a.z, b))
	else
	--b is vec3
		return Vector3.new(op(a, b.x), op(a, b.y), op(a, b.z))
	end
end

function methods.__add(a, b)
	return opAB(a, b, OpFunc.add)
end

function methods.__sub(a, b)
	return opAB(a, b, OpFunc.sub)
end

function methods.__mul(a, b)
	return opAB(a, b, OpFunc.mul)
end

function methods.__div(a, b)
	return opAB(a, b, OpFunc.div)
end

function methods.__unm(a)
	return Vector3.new(-a.x, -a.y, -a.z)
end

function methods.__eq(a, b)
	return isFloatEqual(a.x, b.x) and
			isFloatEqual(a.y, b.y) and
			isFloatEqual(a.z, b.z)
end

function methods.__tostring(a)
	return "[x=" .. tostring(a.x) ..
			",y=" .. tostring(a.y) ..
			",z=" .. tostring(a.z) .. "]"
end
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
