navPoints = {} --global table with navpoints

function loadNavPoints(filename)
	local mapFile = io.open(gamePath..'/prefs/'..filename..'/UIMapMarkers.def', "r");
	if (mapFile) then 
		trace('loading nav points..');
		--		process the file
		local lineCount = 0
		local navPoint = 0
		local navPointsCount = 0
		local currentMap 
		local markerBody = ''		
		while true do
			local line = mapFile:read("*line")			
			if line == nil then break end
			--set map name
			if string.find(line,'string	MapName =') then 
				currentMap = string.match(line, '"(.-)"')
				navPoints[currentMap] = {}
				navPoint = 0
				--trace(currentMap)				
			end	
			if string.find(line,'MAP_MARKER') then 
				lineCount = lineCount + 1
				mapFile:read("*line")--{
				mapFile:read("*line")--	float H = 12
				local marker = mapFile:read("*line")--	string	Image
				marker = string.match(marker, '"(.-)"')
				if string.find(marker,'zzborg') then 					
					marker = 'path'..string.sub(marker,18,19)
					--text is gonna serve as nav point index
					local id = mapFile:read("*line")--	Text = "huu"
					id = string.match(id, '"(.-)"')
					mapFile:read("*line")--	W = 12
					local nx = mapFile:read("*line")--	WorldX = 27654
					nx = string.sub(nx,string.find(nx,'= ')+2)
					local ny = mapFile:read("*line")--	WorldY = 18094
					ny = string.sub(ny,string.find(ny,'= ')+2)
					if navPoints[currentMap][marker] == nil then 
						navPoints[currentMap][marker] = {} 						
					end
					navPoints[currentMap][marker][id] = {}
					navPoints[currentMap][marker][id].x = tostring(nx)
					navPoints[currentMap][marker][id].y = tostring(ny)
					--navPoints[currentMap][marker][navPoint] = { x = nx, y = ny}
					navPoint = navPoint + 1
					navPointsCount = navPointsCount +1
					--navPoints[currentMap] = { tostring(marker) = { x = nx, y = ny}}
					--trace(navPoints[currentMap].x);
					--trace(navPoints[currentMap].y);
				end
			end						
		end
		trace('..done. found '.. tostring(navPointsCount) ..' nav points')
		mapFile:close();
	else 
		trace('failed to load map file!');	
	end	
	return 0
end

function printNavPoints()
	for mapName,paths in pairs(navPoints) do 
		--map name
		trace('  '..tostring(mapName)) 
		for path,points in pairs(paths) do 
			--path id
			trace('     '..tostring(path)) 
			for point,coord in pairs(points) do 
				--nav point id
				trace('         '..tostring(point)) 
				for xy,val in pairs(coord) do
					--nav point values
					trace('            '..tostring(val)) 
				end						
			end		
		end
	end
	return 0
end


