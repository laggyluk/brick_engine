--return states that nodes can take
nsFailed = 0
nsReady = 1
nsRunning = 2
nsSuccess = 3
--togles logic on off by hotkey
aiActive = false
--flags if bot is currently following path
navigating = false
currentNav = {}
pathDirection = -1 --should nav point id's increase or decrease?// 1 for up -1 for down
--flag which keys were down in last frame
keysDown = {}
keysDown['a'] = false;
keysDown['d'] = false;
----holds names of resources that should be gathered
resourcesToGet = {}
followState = nsRunning

--depreciated
function logicUpdateCallback(deltaT)
	local temp = 1+1
	return aiActive
end

--search navPoints table for nearest point to current position 
function findNearestNavPoint()
	local posX,posY = getTracked(playerX,playerY)
	currentNav['x'] = math.huge
	currentNav['y'] = math.huge
	local dist
	local shortest = math.huge
	for mapName,paths in pairs(navPoints) do 
		for path,points in pairs(paths) do 
			for point,coord in pairs(points) do
				dist = distance(posX,coord.x,posY,coord.y)
				if dist < shortest then 
					shortest = dist
					currentNav['x'] = tonumber(coord.x)
					currentNav['y'] = tonumber(coord.y)
					currentNav['point'] = point
					currentNav['path'] = path
					currentNav['map'] = mapName 
				end						
				--trace('x: '..coord.x..' y: '..coord.y..' dist: '..dist)
			end
		end
	end
	return shortest
end

--sets up what resource type is supposed to be gathered. one per line, order is priority
function setResourceType(params)
	--params are strings separated by line breaks. let's convert them to table	
	resourcesToGet = split(params,"\n")	
	trace('setResourceType top: ' .. resourcesToGet[1]);
	return nsSuccess;
end

--selects one of loaded nav paths. params: mapName, pathID, up/down
function setNavPath(params)	
	dummyFollow	= false
	local tbl = getParams(params)
	local posX,posY = getTracked(playerX,playerY)
	local result = nsFailure
	currentNav['x'] = math.huge
	currentNav['y'] = math.huge
	local dist	
	local shortest = math.huge
	if navPoints[tbl[1]][tbl[2]]==nil then
			trace('setNavPath:: path not found');
		else
			for point,coord in pairs(navPoints[tbl[1]][tbl[2]]) do
				dist = distance(posX,coord.x,posY,coord.y)
				if dist < shortest then 
					shortest = dist
					currentNav['x'] = tonumber(coord.x)
					currentNav['y'] = tonumber(coord.y)
					currentNav['point'] = point
					currentNav['path'] = tbl[2]
					currentNav['map'] = tbl[1] 
				end										
				--trace('x: '..coord.x..' y: '..coord.y..' dist: '..dist)
			end
			--set traverse direction
			if tbl[3]=='up' then pathDirection = 1 else pathDirection = -1 end
			result = nsSuccess
		end
	--crash fix
	if navPoints[tbl[1]] == nil then 
		currentNav['x'] = 666
		currentNav['y'] = 666
		trace('setNavPath:: nav points for '..tbl[1]..' don\'t exist in map file')
		result = nsFailed
	end
	--trace('seems ok');
	followState = nsRunning
	return result
end

distanceTimer = 0
--first param is bool, if to look for resources when traveling. second is distance 
--that bot will try to get close to navpoint, optional
function followPath(params)
	local tbl = split(params,"\n")	
	local distTolerance = tonumber(tbl[2])
	if distTolerance == nil then distTolerance=3 end
	if followState==nsRunning then
		--calculate direction to active navpoint
		--get player position
		local posX,posY,rot = getTracked(playerX,playerY,playerRot)
		--calculate angle for facing the navpoint
		--trace(currentNav['y'] ..' '.. posY..' '.. currentNav['x']..' '..posX)
		if tonumber(currentNav['y']) >99999999 then currentNav['y'] = posY end
		if tonumber(currentNav['x']) >99999999 then currentNav['x'] = posX end
		if tonumber(posY) == nil then posY = 0 end
		if tonumber(posX) == nil then posX = 0 end
		angle = math.atan2(currentNav['y'] - posY, currentNav['x'] - posX) * 180 / math.pi;
		angle = angle
		local rot = (rot+90)
		if	rot~=0 then rot = rot % 360 end
		local calc = (angle-rot)		
		if	calc~=0 then calc = calc % 360 end		
		--press keys to face the nav point
		local margin = 17
		if keysDown['d'] == true then keysDown['d']=false end
		if keysDown['a'] == true then keysDown['a']=false end
		
		local dist = distance(posX,currentNav['x'],posY,currentNav['y'])
		--shorter rotation would be left or right?
		if dist>distTolerance then
			if (calc>180) then ---clockwise			
				if (calc<(360-margin)) then 						
					if not keysDown['d'] then
						keyDown('d'); 
						keysDown['d']=true 
						delay(3*math.abs(calc-180))
						keyUp('d');
					end
					--trace('>' .. calc)
				else 			
					keysDown['d'] = false	
				end
			else--rotate counter clockwise	
				if (calc>margin) then 
					if not keysDown['a'] then
						keyDown('a'); 
						delay(3*math.abs(calc))
						keyUp('a');
						keysDown['a']=true 	
					end
					--trace('<' .. calc)
				else 
					keysDown['a'] = false
					--keyUp('a');
					--keyUp('a');
				end
			end		
		end
	--trace(angle..'    '..rot..'   '..calc);
		--no walking when turning
		if not (keysDown['a'] or keysDown['d']) then		
			--if distance from nav point bigger than n run towards it			
			if (dist > distTolerance) then 
				--moze wciskac guzik tylko jezeli pozycja sie nie zmienila od ostatniej klatki
				if not keysDown['w'] then
					keysDown['w'] = true
					keyDown('w');		
				end				
				--if dummyFollow then return nsRunning end
				--if looking for resources ==true then stop after running some distance
				if tbl[1] == 'true' and dummyFollow==false then 
					if os.clock()-distanceTimer>tonumber(getControlValue('eTimedStop')) then
						trace('followPath:: timed stop')
						followState = nsSuccess									
						resetKeys()
						distanceTimer = os.clock()+6666
						return nsSuccess
					end
				end
			else -- find next nav point (or find something else to do)		
				if keysDown['w'] then
					keysDown['w'] = false
					keyUp('w');
				end;
				if dummyFollow then dummyFollow = false; return nsSuccess end
				--keyUp('w');	
				trace('nav point reached')
				--check if it's last point in path. if so then change traverse direction and run back
				local tableSize = 0
				for v,k in pairs(navPoints[currentNav['map']][currentNav['path']]) do tableSize = tableSize +1 end
				currentNav['point'] = tostring(tonumber(currentNav['point']) +pathDirection)				
				if ((tonumber(currentNav['point']) == tableSize) and (pathDirection==1))			
					or (tonumber((currentNav['point'])==0) and (pathDirection==-1)) then
					--tu powinno sprawdzac pierwszy i ostatni punkt a one moga nie byc posortowane
					--albo zaczynac sie od 0 nie od 1
					--if pathDirection==1 then pathDirection=-1 else pathDirection = 1 end
					trace('final nav point reached')

					followState = nsSuccess
					resetKeys()							
				else				
					if navPoints[currentNav['map']][currentNav['path']][currentNav['point']] ==nil then
						--point doesn't exist in table. either there is a gap between point indexes or
						--first / fianal point in path reached
						trace('(nil) nav point reached.')
						navigating = false		
						followState = nsSuccess					
						resetKeys()
						if tbl[1] == 'true' then 
							trace('reverse direction')
							if pathDirection==1 then pathDirection=-1 else pathDirection=1 end
							currentNav['point'] = tostring(tonumber(currentNav['point']) +pathDirection)
							currentNav['x'] = navPoints[currentNav['map']][currentNav['path']][currentNav['point']].x
							currentNav['y'] = navPoints[currentNav['map']][currentNav['path']][currentNav['point']].y									
						end			
						return nsSuccess						
					end
				end
				if followState==nsRunning then
					currentNav['x'] = navPoints[currentNav['map']][currentNav['path']][currentNav['point']].x
					currentNav['y'] = navPoints[currentNav['map']][currentNav['path']][currentNav['point']].y		
					trace('new nav point: '..currentNav['point']..' x: '..currentNav['x']..' y: '..currentNav['y'])
					if tbl[1] == 'true' then 						
						followState = nsSuccess
						--setNextNavPoint()						
						resetKeys()
					end
				end
			end
		else
			if keysDown['w'] then
				keysDown['w'] = false			
				keyUp('w');	
			end
			--trace('!w')
		end
	end
	--trace(tostring(aiActive))
	return followState 
end

function resetTimer()
	distanceTimer = os.clock();
	return nsSuccess
end

function setNextNavPoint()
	followState=nsRunning 
	return nsSuccess
end

--dummy check used to make root node loop run only once
function conditionFail()
	trace('failing on purpose')
	return nsFailed
end

function resetKeys()
	if keysDown['w'] then keyUp('w') keysDown['w'] = false end
	if keysDown['s'] then keyUp('s') keysDown['w'] = false end
	if keysDown['a'] then keyUp('a') keysDown['w'] = false end
	if keysDown['d'] then keyUp('d') keysDown['w'] = false end
end

function getParams(params)
	local tbl = split(params,"\n")	
	for k,v in pairs(tbl) do 
		if string.find(v,'%[')~=nil then			
			tbl[k] = getControlValue(string.sub(v,2,v:len()-1))
		end
	end
	return tbl
end

function split(inputstr, sep)
    if sep == nil then
       sep = "%s"
    end
    t={} ; i=1
    for str in string.gmatch(inputstr, "([^"..sep.."]+)") do		
        t[i] =  str--string.gsub(str, "n", "") 
        i = i + 1
    end
    return t
end

function enableAI()	
	if not initialized then 
		trace('bot not initialized!') 
	else
		resetKeys()
		aiActive = not aiActive
		logicActivate(aiActive)
	end
	return 0
end

local clock = os.clock
function sleep(n)  -- seconds
  local t0 = clock()
  while clock() - t0 <= n do end
end

function distance(x1,x2,y1,y2)
	return math.sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
end