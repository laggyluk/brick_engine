currentTarget = {}
--holds id's of depleted nodes so bot won't target them again
depletedNodes = {}

--target object with given name that is nearest to player position
function targetNearestByName(name)		
	local biot = '0'
	local count = 0
	local result = '0' 
	local dist = math.huge
	--first find string
	local addres = memFindString(name,0)
	while (addres ~= '0' ) do
		--called with base addres and offset as strings to avoid x64 thing
		local resX = memToStr(addres,'808','4','float')
		local resY = memToStr(addres,'812','4','float')	
		--trace('target xy: '..resX..' '..resY);
		--now check if resX is near player. if object is not valid it's gonna be the same result
		local posX,posY = getTracked(playerX,playerY)	
		--trace('player xy: '..posX..' '..posY);
		--when values are not valid this will cause exception
		local d		
		if tonumber(resX) ~= nil and tonumber(resY) ~= nil then 
			d = distance(posX,resX,posY,resY)
			else d = math.huge
		end
		--trace('distance to target: '..d)	
		biot = memToStr(addres,'-4','4','int')					
		if (depletedNodes[biot]==nil) and ( d<dist) then 
			dist = d
			result = biot
			currentTarget['id'] = biot
			currentTarget['dist'] = dist
			currentTarget['x'] = tonumber(resX)
			currentTarget['y'] = tonumber(resY)
			--trace('biotID: '..biot);
			--break early if something found near	
			--if dist<range then break end
		end	
		addres = addStrUInts(addres,'1')
		addres = memFindString(name,addres)				
		count = count+1			
	end
	--convert this mess to float?	
	return result
end

gatheringState = nsReady
gathering = false
inventoryFull = false
dummyFollow = false

function gatherResources(params)
	--first params line is max distance second is tag with memo name that holds data
	local tbl = split(params,"\n")
	tbl[1] = getControlValue(string.sub(tbl[1],2,tbl[1]:len()-1))
	if gatheringState==nsReady then
		if currentTarget['id']=='fail' then return nsFailed end
		--trace('looking for resorce: '..resourcesToGet[1])
		--local biot = targetNearestByName(resourcesToGet[1])
		--trace('nearest resource id: '..currentTarget['id']..' in distance '..currentTarget['dist']);
		gatheringState = nsRunning
		--use path following to get to the destination
		followState=nsRunning		
		currentNav['x'] = currentTarget['x']
		currentNav['y'] = currentTarget['y']
		--currentNav['point'] = 999999
		--currentNav['path'] = 'dummy'
		--currentNav['map'] = 'dummy'
		trace('dummy nav set')
		resetKeys()
		gathering = false
		dummyFollow = true
	end
	local msg
	local posX,posY = getTracked(playerX,playerY)	
	local d = distance(posX,currentTarget['x'],posY,currentTarget['y'])
	if not gathering then
		if d>3 then --need to get neare
			trace('dummy follow')
			followPath('true')			
		else
			resetKeys()
			trace('select node')
			macroCmd('select '..currentTarget['id']);
			msg = getLastChatMsg()
			if (string.find(msg,'There is no')~= nil) then 
				--someone used up the node in meantime?
				trace('node not available')
				depletedNodes[currentTarget['id']] = true
				gatheringState = nsSuccess	
				inventoryFull = false --eh
				gatheringState = nsReady
				followState=nsRunning
				gathering = false
			else
				macroCmd('gather');
				delay(100)
				msg = getLastChatMsg()
				if (string.find(msg,'are too far')~= nil) then
					trace('node too far')
					--depletedNodes[currentTarget['id']] = true
					inventoryFull = false --eh
					gatheringState = nsReady
					currentTarget['id']='fail'
					followState=nsRunning
					gathering = false			
				elseif (string.find(msg,'can not')~= nil) then 
					--equip tool 
					typeStr(tbl[1])
					sleep(1)
					macroCmd('gather');
				else
					gathering = true
					trace('mining')
					followState=nsRunning
				end
			end
		end
	else
		--pick pick pick 
		gatheringState = nsRunning
		--wait for depletion or inventory full
		msg = getLastChatMsg()
		if msg~='' then trace(msg) end
		--check if proer tool is equipped
		if (string.find(msg,'can not')~= nil) then 
			--equip tool 
			typeStr(tbl[1])
			sleep(1)
			macroCmd('gather');
		end
		--check if node is depleted, if so then bot will search for next one
		if (string.find(msg,'depleted')~= nil) then
			--mark id as depleted
			depletedNodes[currentTarget['id']] = true
			gatheringState = nsSuccess	
			inventoryFull = false --eh
			gatheringState = nsReady
			gathering = false
		end
		--inventory full, node fails
		if (string.find(msg,'inventory')~= nil) then
			gatheringState = nsFailed
			inventoryFull = true
			gatheringState = nsReady
			gathering = false
		end	
		if (string.find(msg,'rooted')~= nil) then			
			trace('character rooted!')
			--unbalast()
			gatheringState = nsFailed
			inventoryFull = true
			gatheringState = nsReady
			gathering = false
		end	
	end
	return gatheringState
end

function inventoryCheck()
	if inventoryFull == true then 
		return nsFailed
	else 
		return nsSuccess
	end
end

function targetNearestRes(params)
	local tbl = split(params,"\n")	
	local maxDist = tonumber(getControlValue(string.sub(tbl[1],2,tbl[1]:len()-1)))
	params = getControlValue(string.sub(tbl[2],2,tbl[2]:len()-1))
	tbl = nil 
	tbl = split(params,"\n")
	local size = 1
	for	k,v in pairs(tbl) do 
		tbl[k]=string.gsub(v, "\n", "") 
		size=size+1
	end
	--trace('targetNearestRes params: '.. params)	
	--first param is max allowed distance to target 
	--rest of params i list of resources to get
	local c = 1--current line
	local biot = '0'
	local result = nsRunning
	
	while true do 
		if c>size-1 then break end		
		trace('searching for resource: '.. tbl[c])
		--trace('max distance '.. maxDist)
		local b = targetNearestByName(tbl[c])	
		if (currentTarget['dist']~=nil) then
			--trace('biot: ' .. b)
			--trace('distance ' ..currentTarget['dist'])
			if (currentTarget['dist'] < maxDist and b~='0') then 
				biot = b; 
				--trace('yeah')
				break 
			end			
		end
		trace(tbl[c] .. ' not found')
		c = c+1	
	end
	if biot=='0' then 
		trace('no resoureces found nearby')
		currentTarget['id']='fail'
		result = nsFailed
	else 
		trace('nearest resource id: '.. biot) 
		result = nsSuccess		
	end
	return result
end

function resetVariables()
	navigating = false
	gatheringState = nsReady
	gathering = false
	inventoryFull = false	
	return nsSuccess
end