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
