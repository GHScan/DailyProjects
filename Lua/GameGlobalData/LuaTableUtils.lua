
tableext = {}

function tableext.empty( t )
	return not next(t)
end

function tableext.isarray(t)
    return #t > 0
end

function tableext.merge( dest, src )
	for k,v in pairs(src) do
		dest[k] = v
	end
	return dest
end

function tableext.equal( t1, t2 )
	for k,v in pairs(t1) do
		if v ~= t2[k] then return false end
	end
	for k,v in pairs(t2) do
		if v ~= t1[k] then return false end
	end
	return true
end

function tableext.requal(t, t2)
    for k, v in pairs(t) do
        if type(v) == 'table' then
            local v2 = t2[k]
            if type(v2) ~= 'table' then return false end
            if not tableext.requal(v, v2) then return false end
        else
            if v ~= t2[k] then return false end
        end
    end
    for k, v in pairs(t2) do
        if type(v) == 'table' then
            local v2 = t[k]
            if type(v2) ~= 'table' then return false end
            if not tableext.requal(v, v2) then return false end
        else
            if v ~= t[k] then return false end
        end
    end
    return true
end

function tableext.filter( t, condition )
	local res = {}
	for k, v in pairs(t) do
		if condition(k,v) then
			res[k] = v
		end
	end
	return res
end

function tableext.map( t, func )
	local r = {}
    for k, v in pairs(t) do
        local nk, nv = func(k, v)
        r[nk] = nv
    end
    return r
end

function tableext.copy( t )
	local res = {}
	for k,v in pairs(t) do
		res[k] = v
	end
	return res
end

function tableext.rcopy( t )
	local res = {}
	for k,v in pairs(t) do
		if(type(v)=='table') then
			res[k] = tableext.rcopy(v)
		else
	
			res[k] = v
		end
	end
	return res
end

function tableext.values( t )
	local res = {}
	for _,v in pairs(t) do
		table.insert(res, v)
	end
	return res
end

function tableext.keys( t )
	local res = {}
	for k,_ in pairs(t) do
		table.insert(res, k)
	end
	return res
end

function tableext._tostring(t, depth, buff)
    table.insert(buff, '{\n')
    for k, v in pairs(t) do
        if type(v) == 'table' then
            table.insert(buff,
                string.format('%s[%s] = ', string.rep('\t', depth + 1), utils.repr(k)))
            tableext._tostring(v, depth + 1, buff)
            table.insert(buff, ',\n')
        else
            table.insert(buff, 
                string.format('%s[%s] = %s,\n', string.rep('\t', depth + 1), utils.repr(k), utils.repr(v)))
        end
    end
    table.insert(buff, string.format('%s}', string.rep('\t', depth)))
end

function tableext.tostring(t)
    local buff = {}
    tableext._tostring(t, 0, buff)
    return table.concat(buff, '')
end
