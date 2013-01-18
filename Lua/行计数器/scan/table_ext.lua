--返回尺寸
function table.size(t)
	local cnt = 0
	for _ in pairs(t) do cnt = cnt + 1 end
	return cnt
end

--有序遍历
function table.opairs(t, f)

	local kt = {}
	for k in pairs(t) do
		table.insert(kt, k)
	end
	table.sort(kt, f)

	local iter = function (wt)
		if wt[3] > #wt[2] then return nil end
		wt[3] = wt[3] + 1
		return wt[2][wt[3]], wt[1][wt[2][wt[3]]]
	end

	return iter, {t, kt, 0}
end

-- 浅拷贝; 对比赋值号, 后者只是修改引用
function table.copy(s)
	local d = {}
	for k, v in pairs(s) do
		d[k] = v
	end
	return d
end

-- 深拷贝
function table.dcopy(s)
	local d = {}
	for k, v in pairs(s) do
		if type(v) == "table" then d[k] = table.dcopy(v)
		else d[k] = v end
	end
	return d
end
