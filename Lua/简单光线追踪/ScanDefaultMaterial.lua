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
