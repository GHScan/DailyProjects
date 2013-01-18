require "lfs"

--�淶��һ��·��: ��/�ָ�, ��β����/
function lfs.normalize_path(path)
	path = string.gsub(path, "\\", "/")
	if string.sub(path, -1, -1) == "/" then
		path = string.sub(path, 1, -2)
	end
	return path
end

-- �ݹ�����ļ�/�ļ���
function lfs.dir_recursive(path, enumMode)
	enumMode = enumMode or "f"
	local enumFile = string.match(enumMode, "f")
	local enumDir = string.match(enumMode, "d")

	path = lfs.normalize_path(path)

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
function lfs.split_path(path)
	path = lfs.normalize_path(path)

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
			return path
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

-- �����̷�
function lfs.get_path_drive(path)
	return string.match(path, "^(%a:)")
end

-- �ݹ鴴���ļ���
function lfs.mkdir_recursive(path)
	local dir, name, ext = lfs.split_path(path)
	local partDir = ""
	for i in string.gmatch(dir, "[^/]+") do
		partDir = partDir .. i .. "/"
		lfs.mkdir(partDir)
	end
end
