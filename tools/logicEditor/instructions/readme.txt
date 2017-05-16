1. First open scripts\main.lua in your favourite text editor. Notepad++ for example is pretty nice,
	It has syntax highlight for vatious languages including lua so writing scripts is easier.
	
2. Alter variables in 'setup variables' section of main.lua to match your character
	
3. Custom map markers need to be added to resources. I'm assuming that you have 
	map pack 3.6 installed. 
   Copy .png files from icons folder to 
	resources_override\resources\interface\themes\default\textures
   Overwrite UIMapWindow.def with one supplied
	resources_override\resources\interface\themes\default\defs\UIMapWindow.def
	
4. icons are numbered from 0..9 giving total of 10 possible bot paths on each map.
	it's possible to have 100 different icons with current numbering in case you need more than 
	10. Adding more icons would require altering UIMapWindow.def manually but it's pretty 
	straghtforward.
	
5. creating paths
	paths are lied down in map window using the custom markers (blue face with number)
	each path consists of marker with same number (i.e. path00 would be made of icons with
	0 number). nav points order is decided by marker name. So you add a first marker, name it 0.
	then second named '1' etc. They can be placed in a far distance from each other as long as 
	there is no obstacle on the line. Specially watch out when creating curved parts, it takes 
	a lot of markers to make a non blocked path. It's best to position the character in place
	where marker should be and then open the map and add at the player's icon center


when you have at least one path you can try and test the bot with it.
the current default bot behaviour will do the following:
choos what resource interests it. run along the path00. start digging
you should first alter the 'tree' nodes to fit your character

set what resource to gather. 
	Click on the 'set res type' node. on the right panel click the 'Params list' and edit first
	line to match some resource type. The list will be prioriterized but for now only
	first line is used. Note that all strings are case sensitive so rich & Rich are 
	two different things
	
set path
	Assuming you used '0' markers to set your path leave it as is. 'Params' here are:
	first line is map name that path is assigned to. usually same as current one i guess
	second line is path id in form 'path00..path99'. 
	3rd is traverse direction. 'up' will make bot go along the path from point0 to 
	last one and 'down' will make it go in opposite direction
	
	
follow path
	makes bot to run from nearest nav point in given path to last one. So if you start it
	somwhere in the middle of the path it will look up neares point rather than first one
	
gather
	this one looks up nearest resource to player location. resource type was set in the
	first node. if node is found and out of reach, bot will go in that direction in a 
	straight line. then will start gathering and turn off the logic. 

restart the bot at this point, it will save the changes you made	
default hotkey that toggle 'bot brain' is ctrl+n. 

current issues:
	turning is choppy as you'll notice
	
	if you try to turn the logic before initializing by ctrl+i it most likely will crash
	
	you need to have proper tool already equipped before starting the tree
	
	if you turn it on and bot starts running, it not neceserly will stop when you turn it off. 
	game may see the movement key as still pressed. 
	You can stop the cahracter by pressing movement keys to clear their flags
	
	lot of bugs in the 'tree editor', i'll explain later how it works but that's gonna be some
	topic