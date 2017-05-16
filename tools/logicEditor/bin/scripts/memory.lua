--delphi memory search for given string. result is also a string for x64 bit longint 'simulation'
function memFindString(target,startOffset)
	--create table
	memTable = {}	
	--pass starting offset in first table field. starting offset also passed as string
	memTable[1] = startOffset
	for i=2,string.len(target) do 
		--trace(string.byte(target, i))
		memTable[i] = string.byte(target, i-1)
	end	
	local result = memSearchString(memTable);
	--trace('mem search: ' .. result);
	return result
end

--convert to hex and reverse bytes 
function intToMem(num)
	--hex
	local num=string.format("%x",num)	
	--string.len(num) % 2 ~= 0))
	if (string.len(num) % 2 ~= 0) then num='0'..num end	
	local rev = ''
	for i=string.len(num),1,-2 do 
		rev = rev .. string.sub(num, i-1, i)		
	end
	return rev
end

--convert hex string to string of bytes
function hexToByte(num)	
	local rev = ''
	if (string.len(num) % 2 ~= 0) then num='0'..num end	
	--trace(num)
	for i=1,string.len(num),2 do 
		rev = rev .. string.char(tonumber(string.sub(num, i, i+1), 16))
		--trace(tonumber(string.sub(num, i, i+1), 16)..string.char(tonumber(string.sub(num, i, i+1), 16)))		
		--trace(tonumber(string.sub(num, i, i+1)))
	end
	return rev
end
