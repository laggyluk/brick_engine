hotKeys = {} -- table that holds key - function pairs
-- register a hotkey that will run given function in a form: addHotkey(alt_x,lootAll)
function addHotkey(hot,func)
	hotKeys[tostring(hot)] = func
	--trace('func is '..tostring(func))
	--hotkey needs to be registered for os to catch it
	regHotkey(hot)
	return 0
end

function hotkeyCallback(id)
	--trace('hotkeyCallback '..id)
	hotKeys[tostring(id)]()
	return 0
end