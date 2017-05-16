--string typing helper function 
function macroCmd(macro)
	delay(500)
	keyDown('enter')
	keyUp('enter')
	--delay(2000)
	typeStr('/'..macro)
	keyDown('enter')
	keyUp('enter')
	return 0
end
	
function lootAll()
	targetWindow('Istaria')
	--open loot window for targeted corpse by /loot macro
	macroCmd('loot');
	--wait for window to show up as it is lagged
	mouseMove(424,436)
	if (waitForColor(424,436,0,10)>0) then 
		delay(300)
		--loot window seems to forget where it was last time and always appears in same place and size
		--first check if take all button is active
		if (getColor(448,436)==1317919) then
			--corpse is empty. click 'close' button
			trace('corpse empty :(')
			mouseMove(553,428)
			mouseClick(0);
		else
			--take all button is active so click it
			trace('looting all!')
			mouseMove(448,436)
			for i=0,4 do		
				mouseClick(0)
				delay(100)
				--seems that content of loot window may also arrive with lag so click few times
				--till window closes
				if (getColor(424,436)~=0) then break end
				delay(100)
			end
		end	
	else
		trace('window not found under cursor. looting failed :(')
	end
	return 0
end	

--initialize stuff when game is properly loaded	
function init()
	--setup handles for memory search and stuff
	windowHandle,windowWidth,windowHeight = targetWindow('Istaria')
	if windowHandle==0 then 
		trace('target window not found, stopping') 
	else  
		--find player data offset in memory
		--player biote is integer and needs to be converted to hex and then reversed for mem search
		playerBase = memFindString(hexToByte(intToMem(playerBiote))..playerFirstName..' '..playerLastName,'0')
		if playerBase=='0' then trace('player base offset not found!') else trace('player base offset: '..playerBase) end
		--add values for tracking (offsets are decimal)
		playerX = trackMem(tostring(playerBase+812),'float','player X')
		playerY = trackMem(tostring(playerBase+816),'float','player Y')
		playerZ = trackMem(tostring(playerBase+820),'float','player Z')	
		playerRot = trackMem(tostring(playerBase+832),'float','playerRot')			
		initialized = 1
		addHotkey('shift_x',lootAll)   
		addHotkey('ctrl_n',enableAI)
		addHotkey('alt_v',unbalast)		
	end	
	return 0
end	

--retreives last message that was displayed in main chat. using log file
function getLastChatMsg()
	--trace(getNextLogMessage(chatLogFile))	
	return (getNextLogMessage(chatLogFile))
	--chatLogFile = 'D:/temp/istaria/logs/client_chat_log_03-21-13_15_11_20.txt'
end

function resTest()
	createItem(1)
	return 0
end