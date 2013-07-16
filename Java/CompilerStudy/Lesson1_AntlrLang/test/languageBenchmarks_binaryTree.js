
function BottomUpTree(item, depth) {
  if (depth > 0) {
    var i = item + item
    depth = depth - 1
    var left= BottomUpTree(i-1, depth)
    var right = BottomUpTree(i, depth)
    return [ item, left, right ]
  }
  else {
    return [ item ]
  }
}

function ItemCheck(tree) {
  if (#tree == 3) {
    return tree[0] + ItemCheck(tree[1]) - ItemCheck(tree[2])
  }
  else {
    return tree[0]
  }
}

var N = 15
var mindepth = 4
var maxdepth = mindepth + 2
if (maxdepth < N) { maxdepth = N}

{
  var stretchdepth = maxdepth + 1
  var stretchtree = BottomUpTree(0, stretchdepth)
  print([format('stretch tree of depth %f\t check: %f\n', [stretchdepth, ItemCheck(stretchtree)])])
}

var longlivedtree = BottomUpTree(0, maxdepth)

for (var depth=mindepth; depth <= maxdepth; depth += 2) {
  var iterations = 2 ^ (maxdepth - depth + mindepth)
  var check = 0
  for (var i = 1; i <= iterations; ++i) {
    check = check + ItemCheck(BottomUpTree(1, depth)) +
            ItemCheck(BottomUpTree(-1, depth))
  }
  print([format('%f\t trees of depth %f\t check: %f\n', [iterations*2, depth, check])])
}

print([format('long lived tree of depth %f\t check: %f\n', [maxdepth, ItemCheck(longlivedtree)])])
