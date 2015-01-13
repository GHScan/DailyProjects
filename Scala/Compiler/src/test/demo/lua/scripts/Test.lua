require "scan"

function perf_timer(a)
	local begin = os.clock()
	a()
	return os.clock() - begin
end


function sort_algo_test(algo, algoName, len, loop)
	local t = {}
	for i = 1, len do
		table.insert(t, math.random(len))
		-- table.insert(t, i)
	end

	local tick = 0

	for i = 1, loop do
		local tt = table.copy(t)
		tick = tick + perf_timer(function() algo(tt) end)

		for i = 1, len - 1 do
			if not (tt[i] <= tt[i + 1]) then
				-- print(i, tt[i], tt[i + 1])
				assert(false)
			end
		end
	end

	print(algoName .. " : " .. tick)
end


function quick_sort1(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return end

	local base = a[first]
	local min = first
	local max = last + 1
	while min < max do
		while min + 1 <= last and a[min + 1] <= base do
			min = min + 1
		end
		while a[max - 1] > base do
			max = max - 1
		end
		if min >= max -1 then break end
		min, max = min + 1, max - 1
		a[min], a[max] = a[max], a[min]
	end
	a[min], a[first] = a[first], a[min]

	quick_sort1(a, first, min - 1)
	quick_sort1(a, min + 1, last)
end


function quick_sort2(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return end

	local base = a[first]
	local min = first + 1
	local max = last
	while min <= max do
		while min <= last and a[min] <= base do
			min = min + 1
		end
		while a[max] > base do
			max = max - 1
		end
		if min > max then break end
		a[min], a[max] = a[max], a[min]
		min, max = min + 1, max - 1
	end
	a[max], a[first] = a[first], a[max]

	quick_sort2(a, first, max - 1)
	quick_sort2(a, max + 1, last)
end


function quick_sort3(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return end

	do
		local t = math.floor((first + last) / 2)
		a[t], a[first] = a[first], a[t]
		if a[first] > a[last] then
			a[first], a[last] = a[last], a[first]
		end
	end

	a[last] = a[last] + 1

	local base = a[first]
	local min = first + 1
	local max = last - 1
	while min <= max do
		while a[min] <= base do
			min = min + 1
		end
		while a[max] > base do
			max = max - 1
		end
		if min > max then break end
		a[min], a[max] = a[max], a[min]
		min, max = min + 1, max - 1
	end
	a[max], a[first] = a[first], a[max]

	a[last] = a[last] - 1

	quick_sort3(a, first, max - 1)
	quick_sort3(a, max + 1, last)
end


function equal_pos(a, first, last, val)

	while first < last do
		local mid = math.floor((first + last) / 2)
		if a[mid] < val then
			first = math.min(mid + 1, last)
		elseif a[mid] > val then
			last = math.max(mid - 1, first)
		else --  a[mid] == val
			first, last = mid, mid
			break
		end
	end

	if a[first] <= val then return first + 1
	else return first end
end


function insertion_sort(a, first, last)
	first = first or 1
	last = last or #a

	for i = first + 1, last do
		local base = a[i]
		local pos = equal_pos(a, first, i - 1, base)

		for j = i, pos + 1, -1 do
			a[j] = a[j - 1]
		end
		a[pos] = base
	end
end


function insertion_sort_simple(a, first, last)
	first = first or 1
	last = last or #a

	for i = first + 1, last do
		local base = a[i]
		local j = i - 1
		while j >= first do
			if a[j] <= base then break end
			a[j + 1] = a[j]
			j = j - 1
		end
		a[j + 1] = base
	end
end


function quick_sort4(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return end
	if last - first <= 16 then
		return insertion_sort_simple(a, first, last)
	end

	do
		local t = math.floor((first + last) / 2)
		a[t], a[first] = a[first], a[t]
		if a[first] > a[last] then
			a[first], a[last] = a[last], a[first]
		end
	end

	a[last] = a[last] + 1

	local base = a[first]
	local min = first + 1
	local max = last - 1
	while min <= max do
		while a[min] <= base do
			min = min + 1
		end
		while a[max] > base do
			max = max - 1
		end
		if min > max then break end
		a[min], a[max] = a[max], a[min]
		min, max = min + 1, max - 1
	end
	a[max], a[first] = a[first], a[max]

	a[last] = a[last] - 1

	quick_sort4(a, first, max - 1)
	quick_sort4(a, max + 1, last)
end


function quick_sort5(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return end
	if last - first <= 16 then
		return insertion_sort_simple(a, first, last)
	end

	while first < last do

		if last - first <= 16 then
			return quick_sort5(a, first, last)
		end

		do
			local t = math.floor((first + last) / 2)
			a[t], a[first] = a[first], a[t]
			if a[first] > a[last] then
				a[first], a[last] = a[last], a[first]
			end
		end

		a[last] = a[last] + 1

		local base = a[first]
		local min = first + 1
		local max = last - 1
		while min <= max do
			while a[min] <= base do
				min = min + 1
			end
			while a[max] > base do
				max = max - 1
			end
			if min > max then break end
			a[min], a[max] = a[max], a[min]
			min, max = min + 1, max - 1
		end
		a[max], a[first] = a[first], a[max]

		a[last] = a[last] - 1

		if max - first > last - max then
			quick_sort5(a, max + 1, last)
			last = max - 1
		else
			quick_sort5(a, first, max - 1)
			first = max + 1
		end
	end

end


function push_heap(a, first, last)
	local begin = first - 1
	local len = last - first + 1
	local base = a[last]
	while len > 1 do
		local parent = math.floor(len / 2)
		if a[begin + parent] >= base then break end
		a[begin + len] = a[begin + parent]
		len = parent
	end
	a[begin + len] = base
end


function pop_heap(a, first, last)
	a[first], a[last] = a[last], a[first]
	local len = last - first + 1 - 1
	local test = 1
	local begin = first - 1
	local base = a[begin + test]
	while true do
		local child = test * 2
		if child > len then break end
		if child + 1 <= len and a[begin + child + 1] > a[begin + child] then
			child = child + 1
		end
		if a[begin + child] <= base then break end
		a[begin + test] = a[begin + child]
		test = child
	end
	a[begin + test] = base
end


function heap_sort(a, first, last)
	first = first or 1
	last = last or #a
	for i = first + 1, last do
		push_heap(a, first, i)
	end
	for i = last, first + 1, -1 do
		pop_heap(a, first, i)
	end
end


function quick_sort6(a, first, last)
	first = first or 1
	last = last or #a
	if first >= last then return
	elseif last - first <= 16 then
		return insertion_sort_simple(a, first, last)
	elseif last - first <= 64 then
		return heap_sort(a, first, last)
	end

	do
		local t = math.floor((first + last) / 2)
		a[t], a[first] = a[first], a[t]
		if a[first] > a[last] then
			a[first], a[last] = a[last], a[first]
		end
	end

	a[last] = a[last] + 1

	local base = a[first]
	local min = first + 1
	local max = last - 1
	while min <= max do
		while a[min] <= base do
			min = min + 1
		end
		while a[max] > base do
			max = max - 1
		end
		if min > max then break end
		a[min], a[max] = a[max], a[min]
		min, max = min + 1, max - 1
	end
	a[max], a[first] = a[first], a[max]

	a[last] = a[last] - 1

	quick_sort6(a, first, max - 1)
	quick_sort6(a, max + 1, last)
end


local len, loop = 1000, 1000
-- local len, loop = 10000, 100
-- local len, loop = 100, 10000
math.randomseed(os.time())
sort_algo_test(quick_sort6, "quick_sort6", len, loop)
sort_algo_test(quick_sort5, "quick_sort5", len, loop)
sort_algo_test(quick_sort4, "quick_sort4", len, loop)
sort_algo_test(quick_sort3, "quick_sort3", len, loop)
sort_algo_test(quick_sort2, "quick_sort2", len, loop)
sort_algo_test(quick_sort1, "quick_sort1", len, loop)
sort_algo_test(table.sort, "table.sort", len, loop)

