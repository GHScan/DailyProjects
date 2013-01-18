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
