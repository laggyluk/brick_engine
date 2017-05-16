print('init')
--result values
nsFailed = 0
nsReady = 1
nsRunning = 2
nsSuccess = 3

function someFunc(obj)
	local x,y,z = getPos(obj)
	--print(string.format('actorID: %d, x: %f, y: %f z: %f',obj,x,y,z))
	return nsRunning
end

function checkHunger(obj)
	local wtf = 1+3
	return nsRunning
end

function goEat(obj)
	local wtf = 1+3
	return nsRunning
end

function idle(obj)
	local wtf = 1+3
	return nsRunning
end

function checkUnderAttack(obj)
	local wtf = 1+3
	return nsFailed
end

function checkCanWin(obj)
	local wtf = 1+3
	return nsRunning
end

function defend(obj)
	local wtf = 1+3
	return nsRunning
end

function runAway(obj)
	local wtf = 1+3
	return nsRunning
end

