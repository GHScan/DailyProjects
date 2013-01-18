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

function string.trim(s)
	return string.match(s, "^[%s%c]*(.-)[%s%c]*$")
end

function string.start_with(s, pattern)
	return string.find(s, pattern) == 1
end

function string.end_with(s, pattern)
	return string.match(s, pattern .. "$") ~= nil
end

function string.split(s, sep)
	local pattern = "[^" .. sep .. "]*"
	local t = {}
	for i in string.gmatch(s, pattern) do
		if not string.match(sep, i) then
			table.insert(t, i)
		end
	end
	return t
end
