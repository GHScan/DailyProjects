require "lfs"

--规范化一个路径: 以/分割, 结尾不是/
function normalizePath(path)
	path = string.gsub(path, "\\", "/")
	if string.sub(path, -1, -1) == "/" then
		path = string.sub(path, 1, -2)
	end
	return path
end

--迭代所有子文件/文件夹
function subDir(path, enumMode)
	enumMode = enumMode or "f"
	local enumFile = string.match(enumMode, "f")
	local enumDir = string.match(enumMode, "d")

	path = normalizePath(path)

	local recursiveIter
	recursiveIter =
	function(thisPath)
		-- thisPath一定是normalized
		thisPath = thisPath or path

		if lfs.attributes(thisPath, "mode") == "directory" then
			for subName in lfs.dir(thisPath) do
				if subName ~= ".." and subName ~= "." then
					recursiveIter(thisPath .. "/" .. subName)
				end
			end

			if enumDir then
				coroutine.yield(thisPath)
			end

		else
			if enumFile then
				coroutine.yield(thisPath)
			end
		end
	end

	return coroutine.wrap(recursiveIter)
end

--返回dir, filename, extension
function splitPath(path, normalized)
	if not normalized then
		path = normalizePath(path)
	end

	local rpath = string.reverse(path)
	local sepPos = string.find(rpath, "/")
	local dotPos = string.find(rpath, "%.")

	--(1)
	if sepPos and dotPos and sepPos < dotPos then
		dotPos = nil
	end

	if sepPos then
		sepPos = #path + 1 - sepPos
		if dotPos then
			dotPos = #path + 1 - dotPos

			if sepPos < dotPos then
				return
					string.sub(path, 1, sepPos - 1),
					string.sub(path, sepPos + 1, dotPos - 1),
					string.sub(path, dotPos + 1, -1)
			else
				-- 在(1)处被剔除了
			end

		else
			return
				string.sub(path, 1, sepPos - 1),
				string.sub(path, sepPos + 1, -1),
				nil
		end
	else
		if dotPos then
			dotPos = #path + 1 - dotPos
			return
				nil,
				string.sub(path, 1, dotPos - 1),
				string.sub(path, dotPos + 1, -1)
		else
			return path
		end
	end
end

function getPathDriver(path)
	return string.match(path, "^(%a:)")
end
--序列化
function table.tostring(t)
	local ts = {}
	for k, v in pairs(t) do
		if #ts > 0 then ts[#ts + 1] = "," end
		ts[#ts + 1] = tostring(k) .. "=" .. tostring(v)
	end
	return table.concat(ts)
end

--返回尺寸
function table.size(t)
	local cnt = 0
	for _ in pairs(t) do
		cnt = cnt + 1
	end
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

--init表示最后一个位置, 从后往前找; 如-1
function string.rfind(s, pattern, init, plain)
	plain = plain or false
	init = init or -1
	if init < 0 then init = init + #s + 1 end

	local start = 1
	local first, last
	while true do
		local _first, _last = string.find(s, pattern, start, plain)
		if not _first or _first > _last then break end
		start = _last + 1
		first, last = _first, _last
	end

	return first, last
end