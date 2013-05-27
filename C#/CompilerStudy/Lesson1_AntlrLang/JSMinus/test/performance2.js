//==============================
function partition(array, left, right, pivotIndex) {
	var pivotValue = array[pivotIndex]
    var temp
    temp = array[right]
    array[right] = array[pivotIndex]
    array[pivotIndex] = temp
	var storeIndex = left
    for (var i = left; i <= right - 1; ++i) {
        if (array[i] <= pivotValue) {
            temp = array[i] 
            array[i] = array[storeIndex]
            array[storeIndex] = temp
            storeIndex = storeIndex + 1
        }
    }
    temp = array[storeIndex]
    array[storeIndex] = array[right]
    array[right] = temp
    return storeIndex
}
function quicksort(array, left, right) {
	if (right > left) {
	    var pivotNewIndex = partition(array, left, right, left)
	    quicksort(array, left, pivotNewIndex - 1)
	    quicksort(array, pivotNewIndex + 1, right)
    }
}

function copytable(t)  {
    var r = []
    var n = #t
    for (var i = 0; i < n; ++i) {
        insert(r, v)
    }
    return r
}
//==============================
function permutations(a, i, n) {
    if (i == n) {
        //for (var i = 0; i < #a; ++i) print(a[i])
        //println()
        return
    }
    var temp
    for (var j = i; j < n; ++j) {
        temp = a[i]
        a[i] = a[j]
        a[j] = temp
        permutations(a, i + 1, n)
        temp = a[i]
        a[i] = a[j]
        a[j] = temp
    }
}
//==============================
function bst_insert(t, k, v) {
    if (not t) {
        t = [k, v, nil, nil]
    } else {
        if (t[0] == k) {
            t[1] = v
        }
        else {
            if (k < t[0]) {
                t[2] = bst_insert(t[2], k, v)
            } else {
                t[3] = bst_insert(t[3], k, v)
            }
        }
    }
    return t
}
function bst_query(t, k) {
    if (not t) return
    if (t[0] == k) {
        return t[1]
    } else {
        if (k < t[0]) {
            return bst_query(t[2], k)
        } else {
            return bst_query(t[3], k)
        }
    }
}
//==============================

srand(time())

function test_quicksort(loop, n) {
    var arrays = []
    for (var i = 1; i <= loop; ++i) {
        var a = []
        for (var i = 1; i <= n; ++i) {
            insert(a, random())
        }
        insert(arrays, a)
    }
    var start = clock()
    for (var i = 0; i < loop; ++i) {
        quicksort(arrays[i], 0, n - 1)
    }
    println(format('test_quicksort(loop=%d,n=%d): %f', loop, n, clock() - start))
}

function test_permutations(loop, n) {
    var a = []
    for (var i = 1; i <= n; ++i) insert(a, i)
    var start = clock()
    for (var i = 1; i <= loop; ++i) {
        permutations(a, 0, n)
    }
    println(format('test_permutations(loop=%d,n=%d): %f', loop, n, clock() - start))
}

function test_bst(loop, n) {
    var data = []
    var query = []
    for (var i = 1; i <= n; ++i) insert(data, random() % n)
    for (var i = 1; i <= n; ++i) insert(query, random() % n)
    var start = clock()
    for (var i = 0; i < loop; ++i) {
        var t = nil
        for (var i = 0; i < n; ++i) {
            t = bst_insert(t, data[i], data[i])
        }
        for (var i = 0; i < n; ++i) {
            bst_query(t, query[i])
        }
    }
    println(format('test_bst(loop=%d,n=%d): %f', loop, n, clock() - start))
}

test_quicksort(10, 10000)
test_permutations(30, 8)
test_bst(100, 1000)
