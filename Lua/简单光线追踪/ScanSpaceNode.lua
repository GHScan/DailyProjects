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
