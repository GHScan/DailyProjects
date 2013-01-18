-- 所有运算
ops =
{
	{str="+", f=function(a, b) return a + b end},
	{str="-", f=function(a, b) return a - b end},
	{str="*", f=function(a, b) return a * b end},
	{str="/", f=function(a, b) return a / b end},
}

-- 不满足交换律的
ops_uncomm =
{
	ops[2], ops[4]
}

epsilon = 0.0001
total_point = 24
result_set = {}

local function point24(a, exps, len)
	assert(len >= 1)
	if 	len == 1 and
		math.abs(a[1] - total_point) < epsilon and
		result_set[exps[1]] == nil then
		print(exps[1])
		result_set[exps[1]] = true
		return
	end

	for i = 1, len - 1 do
		for j = i + 1, len do
			-- 处理所有运算
			for _, op in ipairs(ops) do
				local ai, aj = a[i], a[j]
				local expi, expj = exps[i], exps[j]

				a[i] = op.f(ai, aj)
				exps[i] = string.format("(%s%s%s)", expi, op.str, expj)
				a[j], exps[j] = a[len], exps[len]
				point24(a, exps, len - 1)

				a[i], exps[i] = ai, expi
				a[j], exps[j] = aj, expj
			end
		end
	end

	for i = 1, len - 1 do
		for j = i + 1, len do
			-- 单独处理非交换律的
			for _, op in ipairs(ops_uncomm) do
				local ai, aj = a[i], a[j]
				local expi, expj = exps[i], exps[j]

				a[i] = op.f(aj, ai)
				exps[i] = string.format("(%s%s%s)", expj, op.str, expi)
				a[j], exps[j] = a[len], exps[len]
				point24(a, exps, len - 1)

				a[i], exps[i] = ai, expi
				a[j], exps[j] = aj, expj
			end
		end
	end
end

a = {0, 0, 0, 0}
exps = {}
for i, v in ipairs(a) do
	a[i] = io.read("*n")
	exps[i] = tostring(a[i])
end
point24(a, exps, 4)
