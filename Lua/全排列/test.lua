require "scan"

function nextPermutation(a)
	local found = #a
	repeat
		found = found - 1
	until found < 1 or a[found] < a[found + 1]
	if found < 1 then return end

	local newPos = found + 1
	while newPos <= #a and a[newPos] > a[found] do
		newPos = newPos + 1
	end
	--assert((newPos <= #a and a[newPos - 1] > a[found] and a[newPos] < a[found]) or (newPos > #a and a[#a] > a[found]))

	a[newPos - 1], a[found] = a[found], a[newPos - 1]
	table.reverse(a, found + 1)

	return a
end

function permutationIter(a)
	local f = nil
	f =
	function(first)
		first = first or 1
		if first == #a then
			coroutine.yield(a)
			return
		end

		for i = first, #a do
			a[first], a[i] = a[i], a[first]
			f(first + 1)
			a[first], a[i] = a[i], a[first]
		end
	end
	return coroutine.wrap(f)
end

function timer(f)
	local begin = os.clock()
	f()
	print(os.clock() - begin)
end

local a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

timer(
function()
	local aa = table.copy(a)
	repeat
		aa = nextPermutation(aa)
	until not aa
end)

collectgarbage()

timer(
function()
	for aa in permutationIter(table.copy(a)) do
	end
end)

