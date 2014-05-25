##### VCPS

The technical i called vcps is a variant of cps, it used to lazy evaluate an ast. 
the caller drive the evaluation step by step, so, it is also a good skill to iterate a recursive structure.
while it used as an iterator maker, it is more powerful than yield(because it support recursion), almost equallty
as coroutine, but has less overhead(it tracked the stack with a linked closure list, like cps)
