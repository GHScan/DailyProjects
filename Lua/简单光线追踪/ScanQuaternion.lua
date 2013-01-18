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
