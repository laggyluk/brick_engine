unit mega_lua_bindings_win;

interface

uses sendinputhelper,windows,lua, lualib, fmx.dialogs,system.SysUtils, fmx.Platform,
     System.Types, fmx.forms, math, vectorGeometry,direct_input_scan_codes,
     fmx.platform.Win,system.classes,fmx.types,fmx.controls, fmx.edit,
     fmx.listbox, fmx.memo, generics.collections;

//published functions will automatically populate lua functions table
type

  TPrintProc = procedure(const s:string);
  //user can select method for handling keyboard events.
  //virtual key codes or scan codes for directInput
  //helper means using SendInputHelper.pas method
  eKeyboardModes = (kmVKCodes,kmScanCodes,kmHelper);

  //function result must be number of returned variables
  TLuaClass = class (TLua)
  private
    //when ui is created, save how big it was so in case of lua fail we don't overwrite ui with blank file
    configUILines:integer;
    //checks if lua function was called with expected number of params
    function validate(shouldBe:integer):boolean;
    //load 2 ints from lua stack to given vars
    function luaGet2Ints(var x,y:integer):boolean;
    function luaGet1Int(var x:integer):boolean;
    function luaGet3Ints(var x, y,z: integer):boolean;
    function luaGet4Ints(var x, y,z,w: integer):boolean;
    function luaGet1str(var s:string):boolean;
    function luaGet1bool(var b:boolean):boolean;
    function luaGet2str(var s1,s2:string):boolean;
    function luaGet3str(var s1,s2,s3:string):boolean;
    function luaGet4str(var s1,s2,s3,s4:string):boolean;
    function luaGet1int1str(var id:integer;var func:string):boolean;
    //if elses win/mac
    procedure SetMousePos(x,y:integer);
    procedure clickMouse(x,y:integer; button:integer = 0);
    //prints string with current lua line
    procedure printLine(s:string);
    //system dependend cursor pos getter
    procedure getMousePos(var x,y:integer);
    function pressKey(s: string;press,release:boolean): integer;
  public
    // holds state for getNextLogMessage
    lastMessageLine:integer;
    lastMessageFilename:string;
    keyboardMode:eKeyboardModes;
    print:TPrintProc;
    //if true then functions will output some additional info as they are executed
    debugTrace:boolean;
    logicEnabled:boolean;
    //list of ui controls created from script
    controls:TList<TControl>;
    constructor create(autoload:boolean);
    destructor destroy;
    //used by parent form to call a function in lua when hotkey is pressed
    procedure hotkeyCall(const func:string);
    //when enabled, form timer will call this script func to update logic
    function logicUpdate(deltaT:double):boolean;
    //call function with params passed in string. separated by line breaks
    function callFunction(func,params:string):integer;
    //saves ui items from controls list
    procedure saveUIScript(filename:string);
    procedure loadUIScript(filename:string);
  published
    function HelloWorld(state: TLuaState): Integer;
    //set mouse position to given x,y in desktop coordinates
    function mouseSetAbs(State: TLuaState): Integer;
    //set mouse position relative to 'window' position
    function mouseSet(State: TLuaState): Integer;
    //click mouse absolute position
    function mouseClickAbs(State: TLuaState): Integer;
    //click at position relative to targetWindow
    function mouseClick(State: TLuaState): Integer;
    //searches for process name and sets windowPosition relative pos
    //for setMouse calls. window pos should be then tracked for changes
    //actually finds even partial names so watch out
    function targetWindow(s:string):integer;
    //sleep
    function delay(mil:integer):integer;
    //sets focus and brings window to front
    function bringToFront(wnd:cardinal):integer;
    //finds window with given name
    function findWinByName(partialTitle:string):integer;
    //type text and possibly end with enter key
    function typeStr(s:string;enter:boolean):integer;
    //send key down state
    function keyDown(s:string):integer;
    //send key up state
    function keyUp(s:string):integer;
    //smoothly moves mouse cursor lerping position along the way
    function mouseMove:integer;
    //select keyboard events mode
    function selectKeyboardMode:integer;
    //prints string to console
    function trace:integer;
    //clicks can have random randge added to their values in this range
    function mouseRandomClicks:integer;
    //waits for color at given position till timeout
    function waitForColor:integer;
    //check color at given pixel
    function getColor:integer;
    function getColorRGB:integer;
    //register a hotkey in a form alt_x
    function regHotkey:integer;
    //search memory for a string array. targetWindow handle must be set first
    function memSearchString:integer;
    //sets up how often memory reads are up dated in ms
    function memoryRefreshInterval:integer;
    //read some bytes from givven address + offset. strings they are
    //baseAddress,offset,bytesToRead, resultType:float,int,byte,array
    function memToStr:integer;
    //add memory val for tracking  trackMem(address,type,display text)
    function trackMem:integer;
    //get back memory tracked vals. will try to return as many as were passed in call
    function getTracked:integer;
    //bot ai update on/off
    function logicActivate:integer;
    //get next line from log file
    function getNextLogMessage:integer;
    //adds two integers converted from strings
    function addStrUInts:integer;
    //create interface item
    function createControl:integer;
    //retreive ui control value by name
    function getControlValue:integer;
  end;

var
    mouseStep:single = 0.1;

implementation
uses mega_editor_form, mega_win_functions;

{ TLuaClass }

function TLuaClass.addStrUInts: integer;
var s1,s2,r:string;
begin
  if luaget2str(s1,s2) then begin
     r:=inttostr(strtoint(s1)+strtoint(s2));
  end else
      printLine('achtung! bringToFront:: wrong number of arguments.');
  Lua_Pushstring(self.luainstance,pansichar(AnsiString(r)));
  result:=1;
end;

function TLuaClass.bringToFront(wnd: cardinal): integer;
begin
  if luaGet1Int(integer(wnd)) then ForceForegroundWindow(wnd)
  else printLine('achtung! bringToFront:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.callFunction(func,params: string): integer;
begin
  lua_getglobal(self.LuaInstance, pansichar(AnsiString(func)));
  //push arguments
 // params:=StringReplace(params,'&#xA;',';',[rfReplaceAll]);
//  while
  if params <> '' then  begin
     lua_pushstring(self.LuaInstance,pansichar(AnsiString(params)));
     lua_pcall(self.LuaInstance,1,1,0);
  end else begin
     lua_pcall(self.LuaInstance,0,1,0);
  end;
  result:=lua_tointeger(self.LuaInstance,-1);
  lua_pop(self.LuaInstance,1);
end;

procedure TLuaClass.clickMouse(x, y: integer; button:integer = 0);
begin
{  mouse_event(MOUSEEVENTF_LEFTDOWN, x,Y, 0, 0);
  sleep(random(10));
  mouse_event(MOUSEEVENTF_LEFTUP, x, Y, 0, 0);
  sleep(random(10));}
  mouseKlik(random(randomRange.x)+x,random(randomRange.y)+y,button);
end;

constructor TLuaClass.create(autoload:boolean);
begin
  inherited Create(autoload);
  randomize;
  keyboardMode:=kmScanCodes;
  debugTrace:=false;
  controls:=tlist<TControl>.Create;
end;

function TLuaClass.createControl: integer;
var
  argCount,i:integer;
  args:tstringlist;
begin
  argCount:=Lua_GetTop(self.LuaInstance);
  args:=tstringlist.create;
  result:=0;
  try
    for i:=0 to argCount-1 do args.Add(Lua_Tostring(self.LuaInstance, 1+i));
    Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
    //createControl('label','20','20','300','20','text')
    if lowercase(args[0])='label' then
      with tlabel(controls.Items[controls.Add(tlabel.create(formeditor))])do begin
        text:=args[5];
        position.x:=strtofloat(args[1]);
        position.y:=strtofloat(args[2]);
        width:=strtofloat(args[3]);height:=strtofloat(args[4]);
        parent:=formeditor.configTab;
    end;
    //createControl('edit','20','40','100','20','name','text)
    if lowercase(args[0])='edit' then
       with tedit(controls.Items[controls.Add(tedit.create(formeditor))])do begin
        text:=args[6];position.x:=strtofloat(args[1]);position.y:=strtofloat(args[2]);
        width:=strtofloat(args[3]);height:=strtofloat(args[4]);
        name:=args[5];
        parent:=formeditor.configTab;
    end;
 //createControl('combo','x','y','w','h','name','selIndex','item1','item2')
    if lowercase(args[0])='combo' then
       with tcombobox(controls.Items[controls.Add( tcombobox.create(formeditor))])do begin
        position.x:=strtofloat(args[1]);position.y:=strtofloat(args[2]);
        width:=strtofloat(args[3]);height:=strtofloat(args[4]);
        name:=args[5];
        for i:=7 to args.Count-1 do items.Add(args[i]);
        itemindex:=strtoint(args[6]);
        parent:=formeditor.configTab;
    end;
    //createControl('checkbox','x','y','w','h','name','checked','text')
    if lowercase(args[0])='checkbox' then
       with tcheckbox(controls.Items[controls.Add(tcheckbox.create(formeditor))])do begin
        text:=args[7];position.x:=strtofloat(args[1]);position.y:=strtofloat(args[2]);
        width:=strtofloat(args[3]);height:=strtofloat(args[4]);
        name:=args[5];
        OnChange:=formeditor.checkBoxChange;
        isChecked:=true;
        ischecked:=strtobool(args[6]);
        parent:=formeditor.configTab;
    end;
    //createControl('memo','x','y','w','h','name','text','
    if lowercase(args[0])='memo' then
        with tmemo(controls.Items[controls.Add(tmemo.create(formeditor))])do begin
        text:=args[6];position.x:=strtofloat(args[1]);position.y:=strtofloat(args[2]);
        width:=strtofloat(args[3]);height:=strtofloat(args[4]);
        name:=args[5];
        parent:=formeditor.configTab;
        animated:=false;
    end;
  finally
    args.Free;
  end;
end;

function TLuaClass.delay(mil:integer): integer;
var
  c:integer;
begin
  if luaGet1Int(mil) then begin
     if debugTrace then printLine(format('delay(%d)...',[mil]));
     twait(mil);
  end
  else printLine('achtung! delay:: wrong number of arguments.');
  result:=0;
end;

destructor TLuaClass.destroy;
begin
  controls.Free;
end;

function TLuaClass.findWinByName(partialTitle: string): integer;
begin
  result:=0;
  if luaGet1str(partialTitle) then begin
     result:=findWindowByName(partialTitle);
  end;
  Lua_PushInteger(self.luainstance, result);
  result:=1;
end;

function TLuaClass.getColor: integer;
var
  x,y:integer;
begin
  result:=-1;
  //check color at x,y
  if validate(2) then begin
     luaGet2Ints(x,y);
     result:=getColorUnderMouse(windowPosition.X+x,windowPosition.y+y)
  end else
  //check color under mouse
  if validate(0) then begin
     result:=getColorUnderMouse;
  end;
  lua_pushinteger(self.LuaInstance,result);
  result:=1;
end;

function TLuaClass.getColorRGB: integer;
var
  x,y:integer;
begin
  result:=-1;
  //check color at x,y
  if validate(2) then begin
     luaGet2Ints(x,y);
     result:=getColorUnderMouse(windowPosition.X+x,windowPosition.y+y)
  end else
  //check color under mouse
  if validate(0) then begin
     result:=getColorUnderMouse;
  end;
  lua_pushinteger(self.LuaInstance,getrvalue(result));
  lua_pushinteger(self.LuaInstance,getgvalue(result));
  lua_pushinteger(self.LuaInstance,getbvalue(result));
  result:=3;
end;

function TLuaClass.getControlValue: integer;
var s:string;
    c:tcomponent;
begin
  s:='';
  if luaGet1str(s) then begin
     c:=formeditor.FindComponent(s);
     if c is tlabel then s:=(c as tlabel).Text;//? no use for getting label text i guess :P
     if c is tedit then s:=(c as tedit).Text;
     if c is tcombobox then s:=(c as tcombobox).Items[(c as tcombobox).ItemIndex];
     if c is tcheckbox then s:=booltostr((c as tcheckbox).IsChecked);
     if c is tmemo then
        s:=(c as tmemo).Text;
     if s='' then log('achtung! getControlValue:: control not found!');
  end
  else log('achtung! getControlValue:: wrong number of arguments.');
  lua_pushstring(self.LuaInstance,pansichar(AnsiString(s)));
  result:=1;
end;

procedure TLuaClass.getMousePos(var x, y: integer);
var
  p:tpoint;
begin
  getcursorpos(p);
  x:=p.X;
  y:=p.Y;
end;

function TLuaClass.getNextLogMessage: integer;
var filename:string;
    str:tstringlist;
    s:string;
    stream: TStream;
begin
  s:='';
  if luaGet1str(filename) then begin
    try
     str:=tstringlist.Create;
     stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
     str.LoadFromStream(stream);
     //reset last line position if filename changed
     if  lastMessageFilename<>filename then begin
         lastMessageLine:=0;
         ///'scroll' to last line
         lastMessageLine:=str.Count-2;
     end;
     lastMessageFilename:=filename;
     if (str.Count>lastMessageLine) then begin
        s:=str[lastMessageLine];
        inc(lastMessageLine,2);
     end;
    finally
     stream.Free;
     str.Free;
    end;
  end else log('achtung! getNextLogMessage:: wrong number of arguments.');
  lua_pushstring(self.LuaInstance,pansichar(AnsiString(s)));
  result:=1;
end;

function TLuaClass.getTracked: integer;
var i,typ,params:integer;
begin
//return as many elements as were passed in call
  params:=Lua_GetTop(self.LuaInstance);
  for i:=0 to params-1 do begin
  typ:=lua_tointeger(self.LuaInstance, i+1);
  if typ<memvals.count then begin
    case memVals[typ].typ of
      mtInt:Lua_PushInteger(self.LuaInstance, memVals[typ].int);
      mtFloat:Lua_PushNumber(self.LuaInstance, memVals[typ].f);
      mtByte:Lua_PushInteger(self.LuaInstance, memVals[typ].b);
    end;
  end else //item with that index doesn't exist
      Lua_PushInteger(self.LuaInstance, -1);
  end;
  result:=params;
end;

//checks if number of arguments passed from lua is as expected
function TLuaClass.validate(shouldBe:integer):boolean;
begin
  result:= Lua_GetTop(self.LuaInstance)=shouldBe;
end;


function TLuaClass.waitForColor: integer;
var
  x, y, color, timeout: integer;
begin
  result:=0;
  if luaGet4Ints(x,y,color,timeout) then begin
    if WaitForCol(windowPosition.X+x,windowPosition.y+y,color,timeout) then begin
       result:=1;
       if debugTrace then printLine('WaitForCol: found');
    end else
    begin
       result:=0;
       if debugTrace then printLine('WaitForCol: not found');
    end;
  end
  else
    printLine('achtung! waitForColor:: wrong number of arguments.');
  Lua_PushInteger(self.LuaInstance, result);
  result:=1;
end;

function TLuaClass.HelloWorld(State: TLuaState): Integer;
begin
  showmessage('?Hello!');
end;

procedure TLuaClass.hotkeyCall(const func: string);
begin
//  printLine('hotkey pressed!');
  lua_getglobal(self.LuaInstance, 'hotkeyCallback');
  lua_pushstring(self.LuaInstance,pansichar(AnsiString(func)));
  lua_pcall(self.LuaInstance,1,1,0);
  lua_pop(self.LuaInstance,1);
end;

function TLuaClass.logicUpdate(deltaT:double):boolean;
begin
//  printLine('hotkey pressed!');
  lua_getglobal(self.LuaInstance, 'logicUpdateCallback');
  lua_pushnumber (self.LuaInstance,deltaT);
  lua_pcall(self.LuaInstance,1,1,0);
  result:=lua_toboolean(self.LuaInstance,-1);
  logicEnabled:=result;
  lua_pop(self.LuaInstance,1);
end;

function TLuaClass.keyDown(s: string): integer;
begin
  pressKey(s,true,false);
end;

function TLuaClass.keyUp(s: string): integer;
begin
  pressKey(s,false,true);
end;

procedure TLuaClass.loadUIScript(filename: string);
var t:tstringlist;
begin
  t:=tstringlist.create;
  t.LoadFromFile(filename);
  configUILines:=t.Count;
  t.Free;
  dofile(filename);
end;

function TLuaClass.logicActivate: integer;
var b:boolean;
begin
  if luaGet1bool(b) then begin
     logicEnabled:= not logicEnabled;
     formEditor.logicTimer.Enabled:=logicEnabled;
     //reset nodes when enabling logic
     //if logicEnabled then
       formEditor.resetLogic;
     printLine('bot logic enabled: '+booltostr(b,true));
  end else printLine('achtung! logicActivate:: wrong number of parameters');
end;

function TLuaClass.luaGet1bool(var b: boolean): boolean;
begin
  result:=validate(1);
  if result then begin
    b:=lua_toboolean(self.LuaInstance, 1);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet1Int(var x: integer): boolean;
begin
  result:=validate(1);
  if result then begin
    x:=Lua_ToInteger(self.LuaInstance, 1);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet1int1str(var id: integer;
  var func: string): boolean;
begin
  result:=validate(2);
  if result then begin
    id:=Lua_ToInteger(self.LuaInstance, 1);
    func:=Lua_ToString(self.LuaInstance, 2);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet1str(var s: string): boolean;
begin
  result:=validate(1);
  if result then begin
    s:=lua_tostring(self.LuaInstance,1);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet2Ints(var x, y: integer):boolean;
begin
  result:=validate(2);
  if result then begin
    x:=Lua_ToInteger(self.LuaInstance, 1);
    y:=Lua_ToInteger(self.LuaInstance, 2);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet2str(var s1, s2: string): boolean;
begin
  result:=validate(2);
  if result then begin
    s1:=lua_tostring(self.LuaInstance,1);
    s2:=lua_tostring(self.LuaInstance,2);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet3Ints(var x, y,z: integer):boolean;
begin
  result:=validate(3);
  if result then begin
    x:=Lua_ToInteger(self.LuaInstance, 1);
    y:=Lua_ToInteger(self.LuaInstance, 2);
    z:=Lua_ToInteger(self.LuaInstance, 3);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet3str(var s1, s2, s3: string): boolean;
begin
  result:=validate(3);
  if result then begin
    s1:=lua_tostring(self.LuaInstance,1);
    s2:=lua_tostring(self.LuaInstance,2);
    s3:=lua_tostring(self.LuaInstance,3);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.luaGet4str(var s1, s2, s3,s4: string): boolean;
begin
  result:=validate(4);
  if result then begin
    s1:=lua_tostring(self.LuaInstance,1);
    s2:=lua_tostring(self.LuaInstance,2);
    s3:=lua_tostring(self.LuaInstance,3);
    s4:=lua_tostring(self.LuaInstance,4);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;


function TLuaClass.luaGet4Ints(var x, y, z, w: integer): boolean;
begin
  result:=validate(4);
  if result then begin
    x:=Lua_ToInteger(self.LuaInstance, 1);
    y:=Lua_ToInteger(self.LuaInstance, 2);
    z:=Lua_ToInteger(self.LuaInstance, 3);
    w:=Lua_ToInteger(self.LuaInstance, 4);
  end;
  Lua_Pop(self.LuaInstance, Lua_GetTop(self.LuaInstance));
end;

function TLuaClass.memoryRefreshInterval: integer;
var
  i:integer;
begin
  if luaGet1Int(i) then formeditor.memoryTimer.Interval:=i else
     printLine('achtung! memoryRefreshInterval:: wrong number of arguments.');
end;

function TLuaClass.memToStr: integer;
var
  base, offset, count:cardinal;
  s1,typ:string;
  re:TSearchByteArray;
  f:single;
  i:integer;
  b:byte;
begin
  s1:='';
  if validate(4) then try
     base:=strtoint(Lua_ToString(self.LuaInstance, 1));
     offset:=strtoint(Lua_ToString(self.LuaInstance, 2));
     count:=strtoint(Lua_ToString(self.LuaInstance,3));
     typ:=Lua_ToString(self.LuaInstance, 4);
     setlength(re,count);
     memStream.clear;
     //convert this to asked type to save some pain in lua
     if typ='float' then begin
       memRead(base+offset,count,memStream);
       memStream.ReadBuffer(f ,4);
       if isNan(f) then f:=9999999999;
       s1:=floattostr(f);
     end;
     if typ='int' then begin
       memRead(base+offset,count,memStream);
       memStream.ReadBuffer(i ,4);
       s1:=inttostr(i);
     end;
     if typ='byte' then begin
       memRead(base+offset,count,memStream);
       memStream.ReadBuffer(b ,1);
       s1:=inttostr(b);
     end;
     if typ='array' then begin
       memRead(base+offset,count,memStream);
       memStream.ReadBuffer(re[0] ,count);
       copy(s1,0,count);
     end;
     finally

  end else printLine('achtung! memRead:: wrong number of params');
  Lua_Pushstring(self.LuaInstance, pansichar(AnsiString(s1)));
  result:=1;
end;

function TLuaClass.memSearchString: integer;
var
  s:string;
  search:TSearchByteArray;
  i:integer;
  offset:cardinal;
  t:integer;
begin
   s:='0';
  //memSearchString(string);
  if validate(1) then begin
     t:=1;i:=0;
     lua_pushnil(self.LuaInstance);
     while (lua_next(self.LuaInstance,t)>0) do begin
     //first param is the offset to start search from
      if i=0 then offset:=strtoint(lua_tostring(self.LuaInstance,-1)) else
       begin
        setlength(search,length(search)+1);
        search[length(search)-1]:=lua_tointeger(self.LuaInstance,-1);
       end;
      lua_pop(self.LuaInstance,1);
      inc(i);
     end;
     lua_pop(self.LuaInstance,1);
     s:=inttostr(memFindArray(@search,false,offset));
  end;
  lua_pushstring(self.luainstance, pansichar(AnsiString(s)));
  result:=1;
end;

function TLuaClass.mouseClick(State: TLuaState): Integer;
var
  x,y,btn:integer;
begin
  //check for lua overloads
  //mouseClick(button);
  //no x,y given. click where mouse is currently
  if validate(1) then begin
     luaGet1Int(btn);
     getmousepos(x,y);
     if debugTrace then printLine(format('mouseClick(%d)',[btn]));
     clickMouse(x, y,btn);
  end else
  //mouseClick(x,y)
  if validate(2) then begin
     luaGet2Ints(x,y);
     if debugTrace then printLine(format('mouseClick(%d,%d)',[x,y]));
     x:=windowPosition.x+x;
     y:=windowPosition.y+y;
     clickMouse(x, y,0);
  end else
  //mouseClick(x,y,button)
  if validate(3) then begin
     luaGet3Ints(x,y,btn);
     if debugTrace then printLine(format('mouseClick(%d,%d,%d)',[x,y,btn]));
     x:=windowPosition.x+x;
     y:=windowPosition.y+y;
     clickMouse(x, y,btn);
  end else
   printLine('achtung! mouseClick:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.mouseClickAbs(State: TLuaState): Integer;
var
  x,y:integer;
begin
  if luaGet2Ints(x,y) then begin
     SetMousePos(x,y);
     sleep(random(20));
     clickMouse(x,y);
//     SetMousePos(x+round(windowPosition.x), y+round(windowPosition.y))
  end
  else printLine('achtung! mouseClickAbs:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.mouseMove: integer;
var
  x,y:integer;
  xx,yy:integer;
  Pos,newPos,oldPos:TAffineVector;
  t:single;
begin
  if luaGet2Ints(x,y) then begin
     if debugTrace then printLine(format('mouseMove(%d,%d)',[x,y]));
     getmousepos(xx,yy);
     setVector(oldpos,xx,yy,0);
     setVector(newPos,windowPosition.x+x,windowPosition.y+y,0);
     //move mouse to new position
     t:=0;
     while t<=1 do begin
       t:=t+mouseStep;
       pos:=vectorLerp(oldpos,newPos,t);
       x:=round(pos[0]);y:=round(pos[1]);
       SetMousePos(x,y);
       twait(10);
     end;
  end
  else printLine('achtung! mouseMove:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.mouseRandomClicks: integer;
var
  x,y:integer;
begin
  if luaGet2Ints(x,y) then begin
    randomRange.x:=x;
    randomRange.y:=y;
  end
  else printLine('achtung! mouseRandomClicks:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.mouseSet(State: TLuaState): Integer;
var
  x,y: Integer;
begin
  if luaGet2Ints(x,y) then SetMousePos(windowPosition.x-x,windowPosition.y-y)
  else printLine('achtung! setMouse:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.mouseSetAbs(State: TLuaState): Integer;
var
  x,y: Integer;
begin
  if luaGet2Ints(x,y) then SetMousePos(x, y)
  else printLine('achtung! moveMouseAbs:: wrong number of arguments.');
  result:=0;
end;

procedure TLuaClass.printLine(s: string);
var
  d:lua_Debug;
begin
 //returns garbage instead of proper sript line number :(
{  lua_getstack(self.LuaInstance,1,d);
  lua_getinfo(self.LuaInstance,pansichar('nS1'),d);
  print(inttostr(d.currentline)+': '+s);}
  print(s);
end;

function TLuaClass.regHotkey: integer;
var
  id,k:string;
  key:cardinal;
  atom:integer;
  modd:cardinal;
begin
  if luaGet1str(Id) then begin
     id:=lowercase(id);
     if pos('alt',id)>0 then begin
        modd:=MOD_ALT;
        k:=(copy(id,5,1)[1]);
        key:=vkKeyScan(k[1]);
     end else
     if pos('ctrl',id)>0 then begin
        modd:=MOD_CONTROL;
        k:=(copy(id,6,1)[1]);
        key:=vkKeyScan(k[1]);
     end else
     if pos('shift',id)>0 then begin
        modd:=MOD_SHIFT;
        k:=(copy(id,7,1)[1]);
        key:=vkKeyScan(k[1]);
     end else
     if pos('win',id)>0 then begin
        modd:=MOD_WIN;
        k:=(copy(id,6,1)[1]);
        key:=vkKeyScan(k[1]);
     end else
     begin
        modd:=0;
        k:=id;
        key:=vkKeyScan(k[1]);
     end;
     atom:=globalAddAtom(pwidechar('jkal2ds21'+inttostr(hotkeys.Count-1)));
     if registerHotKey(hotkeyGrabber.fMsgHandlerHWND,atom,modd or MOD_NOREPEAT,key) then begin
        printLine(id+' hotkey registered');
        hotkeys.Add(atom,id);
     end
     else
       printline(id+' hotkey already registered');
  end else
      printLine('achtung! registerHotkey:: wrong number of arguments! or sth');
end;

procedure TLuaClass.saveUIScript(filename: string);
var
  str:tstringlist;
  i,j:integer;
  s:string;
begin
  str:=tstringlist.Create;
  for i:=0 to controls.Count-1 do begin
     if controls[i] is tlabel then with tlabel(controls[i]) do
        str.Add(format('createControl(''label'',''%f'',''%f'',''%f'',''%f''',[position.x,position.y,width,height])+','''+text+''')');
     if controls[i] is tedit then with tedit(controls[i]) do
        str.Add(format('createControl(''edit'',''%f'',''%f'',''%f'',''%f''',[position.x,position.y,width,height])+','''+name+''','''+text+''')');
     if controls[i] is tmemo then with tmemo(controls[i]) do
        str.Add(format('createControl(''memo'',''%f'',''%f'',''%f'',''%f''',[position.x,position.y,width,height])+','''+name+''','''+
        StringReplace(StringReplace(text, #10, '', [rfReplaceAll]), #13, '\n', [rfReplaceAll])+
        ''')');
     if controls[i] is tcombobox then with tcombobox(controls[i]) do begin
        s:='';
        for j:=0 to items.Count-1 do s:=s+'","'+Items[j];
        str.Add(format('createControl(''combo'',''%f'',''%f'',''%f'',''%f''',[position.x,position.y,width,height])+','''+name+''',"'+inttostr(itemindex)+s+'")');
     end;
     if controls[i] is tcheckbox then with tcheckbox(controls[i]) do
        str.Add(format('createControl(''checkbox'',''%f'',''%f'',''%f'',''%f''',[position.x,position.y,width,height])+','''+name+''','''+booltostr(ischecked)+''','''+text+''')');

  end;
  if str.count>0 then str.SaveToFile(filename);
  str.Free;
end;

function TLuaClass.selectKeyboardMode: integer;
var
  s:string;
begin
  if luaGet1str(s) then begin
     if s='virtualKeys' then keyboardMode:=kmVKCodes
     else if s='scanCodes' then keyboardMode:=kmScanCodes
     else printLine('achtung! selectKeyboardMode:: unrecognized parameter');
  end
  else printLine('achtung! selectKeyboardMode:: wrong number of arguments.');
end;

procedure TLuaClass.SetMousePos(x, y: integer);
begin
  //SetCursorPos(x,y);
  mouseMoveTo(x,y);
//  mouseMoveMsg(windowHandle,x,y);
  //setcursorpos(x,y);
  sleep(20);
  //printLine(format('mouseSet(%d,%d)',[x,y]));
end;

//returns handle and window size
function TLuaClass.targetWindow(s: string): integer;
var
  h:cardinal;
  x,y:integer;
  WindowRect: TRect;
begin
  if luaGet1str(s) then begin
    // h:=targetwin(s); ekhem, copyrights
    h:=targetwin('Istaria',x,y);
  end else
     printLine('achtung! targetWindow:: wrong number of arguments.');
  Lua_PushInteger(self.luainstance, h);
  Lua_PushInteger(self.luainstance, x);
  Lua_PushInteger(self.luainstance, y);
  result:=3;
end;

function TLuaClass.trace: integer;
var
  s:string;
begin
  if luaGet1str(s) then printLine(s)
  else printLine('achtung! trace:: wrong number of arguments.');
end;

function TLuaClass.trackMem: integer;
var
  offset,typ,text:string;
  r:^rMemValue;
  index:integer;
  i:integer;
begin
  index:=-1;
  if luaGet3str(offset,typ,text) then begin
  //first chekc if given addres is not tracked already
    for i:=0 to memvals.Count-1 do if memVals[i].offset = strtoint(offset) then begin
        index:=i;
        break;
    end;
    //if addres not tracked then add new record
    if index=-1 then begin
     GetMem(r, SizeOf(rMemValue));
     zeroMemory(r,sizeof( rMemValue));
     r.offset:=strtoint(offset);
     r.text:=text;
     if typ='float' then r.typ:=mtFloat;
     if typ='integer' then r.typ:=mtInt;
     if typ='byte' then r.typ:=mtByte;
     index:=memVals.Add(r^);
    end;
  end;
  lua_pushinteger(self.LuaInstance,index);
  result:=1;
end;

function TLuaClass.typeStr(s: string; enter: boolean): integer;
var
  ih:TSendInputHelper;
begin
  if validate(1) then begin
     s:=lua_tostring(self.LuaInstance,1);
     if keyboardMode=kmVKCodes then sendVKeys(s,true,true);
     if keyboardMode=kmScanCodes then sendScanKeys(s,true,true);
     if keyboardMode=kmHelper then begin
       ih:=TSendInputHelper.Create;
       ih.AddText(s,false);
       ih.AddDelay(100);
       ih.flush;
       ih.Free;
     end;
  end else
      printLine('achtung! typeStr:: wrong number of arguments.');
  result:=0;
end;

function TLuaClass.pressKey(s: string;press,release:boolean): integer;
var
  ih:TSendInputHelper;
  flag:shortint;
begin
  if luaGet1str(s) then begin
     ih:=TSendInputHelper.Create;
     s:=lowercase(s);
//can be single letter or special key: enter, ctrl, alt, shift, win
     //check if normal key or special
     if keyboardMode=kmVKCodes then begin
       if length(s)>1 then begin
          if s='enter' then ih.AddVirtualKey(vk_return,press,release);
          if s='ctrl' then ih.AddVirtualKey(VK_CONTROL,press,release);
          if s='alt' then ih.AddShift([sendinputhelper.ssAlt],press,release);
          if s='win' then ih.AddShift([sendinputhelper.ssWin],press,release);
          if s='shift' then ih.AddShift([sendinputhelper.ssShift],press,release);
       end else
           ih.AddChar(s[1],true,false);
     end;
     if keyboardMode=kmScanCodes then
     begin //send scan codes instead
       flag:=KEYEVENTF_SCANCODE;
       if release then flag:=flag  or KEYEVENTF_KEYUP;
       if length(s)>1 then begin
          if s='enter' then ih.AddKeyboardInput(vk_return,VKToDIK(vk_return),flag,0);
          if s='ctrl' then  ih.AddKeyboardInput(vk_control,VKToDIK(vk_control),flag,0);
          if s='alt' then  ih.AddKeyboardInput(vk_menu,VKToDIK(vk_menu),flag,0);
          if s='win' then  ih.AddKeyboardInput(vk_LWin,VKToDIK(vk_LWin),flag,0);
          if s='shift' then  ih.AddKeyboardInput(vk_shift,VKToDIK(vk_shift),flag,0);
       end else
           //ih.AddKeyboardInput(VKToDIK(ord(s[1])),VKToDIK(ord(s[1])),flag,0);
           sendScanKeys(s,press,release);
//           ih.AddChar(s[1],true,false);
     end;
     ih.AddDelay(50);
     ih.flush;
     ih.Free;
  end else
      printLine('achtung! pressKey:: wrong number of arguments.');
  result:=0;
end;


end.
