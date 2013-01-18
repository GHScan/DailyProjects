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
