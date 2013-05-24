--==============================
function partition(array, left, right, pivotIndex)
	local pivotValue = array[pivotIndex]
	array[pivotIndex], array[right] = array[right], array[pivotIndex]
	local storeIndex = left
	for i =  left, right-1 do
    	if array[i] <= pivotValue then
	        array[i], array[storeIndex] = array[storeIndex], array[i]
	        storeIndex = storeIndex + 1
		end
	end
    array[storeIndex], array[right] = array[right], array[storeIndex]
   return storeIndex
end
function quicksort(array, left, right)
	if right > left then
	    local pivotNewIndex = partition(array, left, right, left)
	    quicksort(array, left, pivotNewIndex - 1)
	    quicksort(array, pivotNewIndex + 1, right)
	end
end

function copytable(t) 
    local r = {}
    for k, v in pairs(t) do
        r[k] = v
    end
    return r
end
--==============================
function permutations(a, i, n)
    if i == n then
        --print(unpack(a))
        return
    end
    for j = i, n do
        a[i], a[j] = a[j], a[i]
        permutations(a, i + 1, n)
        a[i], a[j] = a[j], a[i]
    end
end
--==============================
function bst_insert(t, k, v)
    if not t then 
        t = {k, v, nil, nil}
    else 
        if t[1] == k then 
            t[2] = v
        elseif k < t[1] then
            t[3] = bst_insert(t[3], k, v)
        else
            t[4] = bst_insert(t[4], k, v)
        end
    end
    return t
end
function bst_query(t, k)
    if not t then return end
    if t[1] == k then 
        return t[2]
    elseif k < t[1] then
        return bst_query(t[3], k)
    else
        return bst_query(t[4], k)
    end
end
--==============================

math.randomseed(os.time())

function test_quicksort(loop, n)
    local arrays = {}
    for i = 1, loop do
        local a = {}
        for i = 1, n do
            table.insert(a, math.random())
        end
        table.insert(arrays, a)
    end
    local start = os.clock()
    for _, a in ipairs(arrays) do
        quicksort(a, 1, n)
    end
    print(string.format('test_quicksort(loop=%d,n=%d): %f', loop, n, os.clock() - start))
end

function test_permutations(loop, n)
    local a = {}
    for i = 1, n do table.insert(a, i) end
    local start = os.clock()
    for i = 1, loop do
        permutations(a, 1, n)
    end
    print(string.format('test_permutations(loop=%d,n=%d): %f', loop, n, os.clock() - start))
end

function test_bst(loop, n)
    local data = {}
    local query = {}
    for i = 1, n do table.insert(data, math.random() % n) end
    for i = 1, n do table.insert(query, math.random() % n) end
    local start = os.clock()
    for i = 1, loop do
        local t = nil
        for i = 1, n do
            t = bst_insert(t, data[i], data[i])
        end
        for i = 1, n do
            bst_query(t, query[i])
        end
    end
    print(string.format('test_bst(loop=%d,n=%d): %f', loop, n, os.clock() - start))
end

test_quicksort(10, 10000)
test_permutations(30, 8)
test_bst(100, 1000)
