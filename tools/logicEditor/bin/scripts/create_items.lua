windows = {}

--filters for given formula, clicks it and then clicks n-th item in the 'select item window '
function createItem(params)
	local tbl = split(params,"\n")
	--local deconstruct = getControlValue(string.sub(tbl[1],2,tbl[1]:len()-1))
	local formula = getControlValue(string.sub(tbl[1],2,tbl[1]:len()-1))
	getWindowDimensions()
	--formula hotkey step 1	
	--trace(formula)	
	typeStr(formula)	
	--turn on machines and stuff
	--click tool
	delay(1000)
	mouseMove(windows['ix']+178,windows['iy']+122)
	delay(100)
	mouseClick(0)
	delay(100)
	mouseClick(0)	
	delay(200)
	--click machine
	mouseMove(windows['ix']+178,windows['iy']+162)
	delay(100)
	mouseClick(0)
	delay(100)
	mouseClick(0)
	delay(200)
	--max out qty
	mouseMove(windows['ix']+178,windows['iy']+206)
	delay(100)
	mouseClick(0)
	delay(100)
	mouseClick(0)	
	delay(200)
	--	wait for create button to activate	
	delay(600)
	return nsSuccess
end

function clickDeconstruct()
	mouseMove(windows['ix']+windows['iw']-376,windows['iy']+windows['ih']-20)
	delay(100)
	mouseClick(0)
	return nsSuccess
end

function clickCreate()
	local result = nsFailure
	--check for the red rectangle
	mouseMove(windows['ix']+18,windows['iy']+444)
	delay(200)
	--getColor(windows['ix']+18,windows['iy']+444))
	--local color = getColor(windows['ix']+18,windows['iy']+444)
	local r,g,b = getColorRGB(windows['ix']+18,windows['iy']+444)
	trace('color found: '.. r ..' '.. g .. ' ' .. b)
	--90,30,30
	if (r<50) then
		--if not red then click create
		mouseMove(windows['ix']+windows['iw']-102,windows['iy']+windows['ih']-20)
		delay(100)
		mouseClick(0)
		delay(100)
		mouseClick(0)
		result = nsSuccess
		trace("item created")
	else
		trace("couldn't create item")
	end	
	delay(100)
	macroCmd('closelastwindow') 	
	delay(200)
	local msg = getLastChatMsg()
	return result
end

function moveToMachineCheck()
	local result
	local tableSize = 0
	for v,k in pairs(navPoints[currentNav['map']][currentNav['path']]) do tableSize = tableSize +1 end	
	if tableSize==1 then result = nsFailed else result = nsSuccess end
	return result
end
--open UIPersonal.def and check where and what size knowledge and itemCreation windows
function getWindowDimensions()	
	local file = io.open(gamePath..'/prefs/'..worldName..'_'..playerFirstName..'/UIPersonal.def', "r");
	if (file) then 		
		local found = 0
		local w,h,x,y
		while true do
			local line = file:read("*line")			
			if line == nil then break end					
			if (string.find(string.lower(line),'knowledgewindow')) then 
				--'k' prefix for knwoledge window, 'i' for creation window			
				while string.find(line,'}')==nil do
					line = file:read("*line")--{
					if string.find(line,'SizedH') then
						h = string.sub(line,string.find(line,'= ')+2)
						windows['kh']= h
					end
					if string.find(line,'SizedW') then
						w = string.sub(line,string.find(line,'= ')+2)
						windows['kw']= w
					end
					if string.find(line,'float	X') then
						x = string.sub(line,string.find(line,'= ')+2)
						windows['kx']= x
					end						
					if string.find(line,'float	Y') then
						y = string.sub(line,string.find(line,'= ')+2)
						windows['ky']= y
					end	
				end
				trace('knowledge win: '..' '.. x ..' '.. y ..' '.. w ..' '.. h)	
				if found>2 then break end
			end
			if (string.find(string.lower(line),'inventorywindow')) then 
				--'k' prefix for inventry window, 'i' for creation window			
				while string.find(line,'}')==nil do
					line = file:read("*line")--{
					if string.find(line,'SizedH') then
						h = string.sub(line,string.find(line,'= ')+2)
						windows['inh']= h
					end
					if string.find(line,'SizedW') then
						w = string.sub(line,string.find(line,'= ')+2)
						windows['inw']= w
					end
					if string.find(line,'float	X') then
						x = string.sub(line,string.find(line,'= ')+2)
						windows['inx']= x
					end						
					if string.find(line,'float	Y') then
						y = string.sub(line,string.find(line,'= ')+2)
						windows['iny']= y
					end	
				end
				trace('inventory win: '..' '.. x ..' '.. y ..' '.. w ..' '.. h)	
				if found>2 then break end
			end			
			if (string.find(string.lower(line),'itemcreatewindow')) then 
				--'k' prefix for knwoledge window, 'i' for creation window			
				file:read("*line")--{
				file:read("*line")--float	Alpha = 1
				file:read("*line")--float	BorderAlpha = 1
				file:read("*line")--bool	BorderOn = true
				file:read("*line")--	CloseOnCreate = 30
				file:read("*line")--bool	Locked = false
				file:read("*line")--bool	Minimized = false
				file:read("*line")--float	MinimizeSaveX = 200
				file:read("*line")--float	MinimizeSaveY = 125
				file:read("*line")--float	NormalHeight = 602
				file:read("*line")--float	NormalWidth = 649
				w = file:read("*line")--float	SizedH = 602
				w = string.sub(w,string.find(w,'= ')+2)
				windows['ih']= w
				h = file:read("*line")--float	SizedH = 602
				h = string.sub(h,string.find(h,'= ')+2)
				file:read("*line")
				file:read("*line")
				windows['iw']= h
				x = file:read("*line")
				x = string.sub(x,string.find(x,'= ')+2)
				windows['ix']= x
				y = file:read("*line")--float	SizedH = 602
				y = string.sub(y,string.find(y,'= ')+2)
				windows['iy']= y
				--found = found+1				
				trace('ItemCreate win: '..' '.. x ..' '.. y ..' '.. w ..' '.. h)
				if found>2 then break end
			end		
			if (string.find(string.lower(line),'productselectionwindow')) then 
				--trace('aaaa')
				--'k' prefix for knwoledge window, 'i' for creation window			
				file:read("*line")--{
				file:read("*line")--float	Alpha = 1
				file:read("*line")--float	BorderAlpha = 1
				file:read("*line")--bool	BorderOn = true
				file:read("*line")--bool	Locked = false
				file:read("*line")--bool	Minimized = false
				file:read("*line")--float	MinimizeSaveX = 200
				file:read("*line")--float	MinimizeSaveY = 125
				file:read("*line")--float	NormalHeight = 602
				file:read("*line")--float	NormalWidth = 649
				w = file:read("*line")--float	SizedH = 602
				w = string.sub(w,string.find(w,'= ')+2)
				windows['ph']= w
				h = file:read("*line")--float	SizedH = 602
				h = string.sub(h,string.find(h,'= ')+2)
				file:read("*line")
				file:read("*line")
				windows['pw']= h
				x = file:read("*line")
				x = string.sub(x,string.find(x,'= ')+2)
				windows['px']= x
				y = file:read("*line")--float	SizedH = 602
				y = string.sub(y,string.find(y,'= ')+2)
				windows['py']= y
				--found = found+1				
				trace('Product Select win: '..' '.. x ..' '.. y ..' '.. w ..' '.. h)
				if found>2 then break end
			end		
		end
		file:close();
	else
		io.close()
	end
end