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
