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
