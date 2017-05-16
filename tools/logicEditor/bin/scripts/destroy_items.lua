
--finds item stack with given qty. if qty=0 then finds first stack
function findItemStack(itemName,qty)
	local itemBase = memFindString(itemName,'0')
	local itemX 
	local itemY 
	local itemQty 	
	local c = 0
	local match = false
	while true do 
		local d = memFindString('icon_',itemBase)
			if d-itemBase==116 then
				trace('stack item base: '.. itemBase)
				--item position in the inventory columns/rows starting from 1
				itemX = memToStr(itemBase,'88','4','float')
				itemY = memToStr(itemBase,'92','4','float')
				itemQty = memToStr(itemBase,'216','4','int')
				trace('item inventory position: '.. itemX ..' '.. itemY)				
				trace('stack size: '.. itemQty)
				if itemQty==qty or qty==0 then 
					trace('item qty match')
					match = true
					break 
				end
			end
			itemBase = memFindString(itemName,itemBase+2)
			c = c +1
			if c>100 then break end --infinite loop guard
			if d=='0' then break end
		end
	return match,itemBase,itemX,itemY,itemQty
end

function overburdenCheck()
	--try to move and trigger 'rooted' message
	keyDown('w')
	delay(100)
	keyUp('w')
	delay(500)
	local msg = getLastChatMsg()
	local result = nsFailed
	--are rooted vs no more rooted?
	if (string.find(msg,'cannot currently move')~= nil) then 
		result = nsSuccess 
		trace('char is overburdened')
	else 
		trace('char is not overburdened')
	end	
	return result
end
--looks up resource in inventory, splits it and destroys to be able to move again
function unbalast()
	--check if overburned
	local result = nsSuccess
	--are rooted vs no more rooted?
	
		trace('unbalast..')
		--open inventory window
		macroCmd('window inventorywindow')
		--look up item in memory
		local itemName = getControlValue('eBalast')
		trace(itemName)
		local c = 0
		if itemName ~= nil then 
			local itemBase = memFindString(itemName,'0')
			while true do 
				local d = memFindString('icon_',itemBase)
				if d-itemBase==116 then
					trace('resource item base: '.. itemBase)
					--item position in the inventory columns/rows starting from 1
					local itemX = memToStr(itemBase,'88','4','float')
					local itemY = memToStr(itemBase,'92','4','float')
					local itemQty = memToStr(itemBase,'216','4','int')
					trace('item inventory position: '.. itemX ..' '.. itemY)				
					trace('stack size: '.. itemQty)
					--get window position
					getWindowDimensions()
					--movemouse over the item and select it
					mouseMove(windows['inx']+35+(24*(itemX-1)),windows['iny']+57+(24*(itemY-1)))
					delay(100)
					mouseClick(0)
					delay(100)
					mouseClick(0)
					delay(100)
					--need to split?
					if tonumber(itemQty)>11 then 
						--move mouse over split btn
						mouseMove(windows['inx']+windows['inw']-33,windows['iny']+30)
						delay(100)
						mouseClick(0)
						delay(300)				
						--split 11 items
						keyDown('1')
						keyUp('1')
						keyDown('enter')
						keyUp('enter')	
						delay(200)
						--find splitted stack				
						match,itemBase,itemX,itemY,itemQty = findItemStack(itemName,'11')
					else
						match = true
					end
					if match then 
						trace('deleting splitted stack')
						--select splited stack
						mouseMove(windows['inx']+35+(24*(itemX-1)),windows['iny']+57+(24*(itemY-1)))
						delay(100)
						mouseClick(0)
						delay(100)
						mouseClick(0)						
						--click del btn
						delay(100)
						mouseMove(windows['inx']+windows['inw']-16,windows['iny']+30)
						delay(100)
						mouseClick(0)					
						delay(100)
						mouseClick(0)						
						--move mouse over destroy btn and click
						delay(100)
						mouseMove(windowWidth / 2 + 48,windowHeight / 2 + 90)
						delay(100)
						mouseClick(0)
						delay(500)
						--check if not longer rooted?					
					else 
						track('stack not found. deleting items canceled')
					end
					break 
				end
				itemBase = memFindString(itemName,itemBase+2)
				c = c +1
				if c>100 then break end --infinite loop guard
			end
		end
		--close inv window
		macroCmd('closelastwindow')
	--if (string.find(msg,'There is no')~= nil) then 		
	return result
end