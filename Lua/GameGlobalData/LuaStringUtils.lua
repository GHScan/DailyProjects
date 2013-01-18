
stringext = {}

function stringext.split_valid(str, sep)
	assert(str and sep and #sep > 0)
    local res = {}
    for i in string.gmatch(str, string.format('[^%s]+', sep)) do
    	table.insert(res, i)
    end
    return res
end

function stringext.split(str, sep)
    assert(str and sep and #sep > 0)
    if #str == 0 then return {} end
    local reg = string.format('[%s]', sep)

    local r = {}
    local _begin = 1
    while _begin <= #str do
        local _end = string.find(str, reg, _begin) or #str + 1
        table.insert(r, string.sub(str, _begin, _end - 1))
        _begin = _end + 1
    end
    if string.match(string.sub(str, #str, #str), reg) then table.insert(r, '') end
    return r
end

function stringext.startswith(str, prefix)
    return string.find(str, prefix) == 1
end

function stringext.endswith(str, suffix)
    return stringext.startswith(string.reverse(str), string.reverse(suffix))
end

function stringext.rfind( str, match )
	local rstr = string.reverse( str )
	local rmatch = string.reverse( match )
	local index = string.find(rstr,rmatch)
	if(index ~= nil) then
		return #str - (index + #match - 2)
	end
	return nil
end


function stringext.count( str, match )
	local num = 0
	for i in string.gmatch(str, match) do
		num = num + 1
	end
	return num
end

function stringext.isalpha(str)
	return stringext._ismatch(str,'[%a]+')
end


function stringext.isdigit(str)
	return stringext._ismatch(str,'[%d]+')
end


function stringext.islower(str)
	return stringext._ismatch(str,'[%l]+')
end


function stringext.isupper(str)
	return stringext._ismatch(str,'[%u]+')
end


function stringext.erase_range(str, istart, iend)
	local length = #str
	if(istart == nil) then istart = 1 end
	if(iend == nil) then iend = length end
	if(istart < 0) then istart = length+istart+1 end
	if(iend < 0) then iend = length+iend+1 end
	local str_f = ''
	local str_b = ''
	if(istart-1>0) then str_f = string.sub(str,1,istart-1) end
	if(iend<#str) then str_b = string.sub(str,iend+1) end
	return str_f .. str_b
end

function stringext.trim(s)
    local first = string.find(s, '%S')
    if not first then return '' end
    local last = string.find(string.reverse(s), '%S')
    return string.sub(s, first, #s + 1 - last)
end


function stringext.ljust( str, width, fillchar )
	local difflen = width - #str
	if( difflen < 0 ) then
		return string.sub( str, 1, width )
	elseif( difflen > 0 ) then
		return str .. string.rep( fillchar, difflen )
	else
		return str
	end
end


function stringext.rjust( str, width, fillchar )
	local strlen = #str
	local difflen = width - strlen
	if( difflen < 0 ) then
		return string.sub( str, -difflen+1, strlen )
	elseif( difflen > 0 ) then
		return string.rep( fillchar, difflen ) .. str
	else
		return str
	end
end


function stringext.center( str, width, fill )
	local strlen = #str
	local difflen = width - strlen
	if( difflen < 0 ) then
		difflen = -difflen
		local offset = math.modf(difflen*0.5)
		local sleft = 1 + math.modf( offset )
		return string.sub( str, sleft, sleft+width-1 )
	elseif( difflen > 0 ) then
		local lc, _t = math.modf(difflen*0.5)
		local rc = lc
		if _t ~= 0 then rc = rc + 1 end 
		return string.rep( fill, lc ) .. str .. string.rep(fill, rc)
	else
		return str
	end
end

function stringext._ismatch(str, match)
    local r = string.match(str, match)
    return r ~= nil and #r == #str
end

function stringext.splitext(path)
    local tokens = stringext.split_valid(path, '.')
    local ext = tokens[#tokens]
    table.remove(tokens)
    assert(ext)
    return {table.concat(tokens, '.'), ext}
end
