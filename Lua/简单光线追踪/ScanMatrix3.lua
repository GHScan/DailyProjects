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
