require "lfs"

--�淶��һ��·��: ��/�ָ�, ��β����/
function normalizePath(path)
	path = string.gsub(path, "\\", "/")
	if string.sub(path, -1, -1) == "/" then
		path = string.sub(path, 1, -2)
	end
	return path
end

--�����������ļ�/�ļ���
function subDir(path, enumMode)
	enumMode = enumMode or "f"
	local enumFile = string.match(enumMode, "f")
	local enumDir = string.match(enumMode, "d")

	path = normalizePath(path)

	local recursiveIter
	recursiveIter =
	function(thisPath)
		-- thisPathһ����normalized
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

--����dir, filename, extension
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
				-- ��(1)�����޳���
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
