unit mega_win_functions;

interface
uses winapi.windows,system.SysUtils,system.classes,fmx.types,fmx.forms,
     fmx.platform,system.types,messages,direct_input_scan_codes,vcl.graphics
     ,psapi,TlHelp32;

type
    //timer event for twait
   TEventHandlers = class // create a dummy class
       procedure ontime(Sender: TObject) ;
   end;
   //determine if system is 64bit
   WinIsWow64 = function( Handle: THandle; var Iret: BOOL ): BOOL; stdcall;
   TSearchByteArray = array of byte;
   pSearchByteArray = ^TSearchByteArray;

{$IF DEFINED(CLR)}
  TCreateParams = record
    Caption: string;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWND;
    Param: IntPtr;
    WindowClass: TWndClassInfo;
    WndProc: TFNWndProc;
    WinClassName: string;
  end;
{$ELSE}
  TCreateParams = record
    Caption: PChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;
{$ENDIF}

  function findWindowByName(partialTitle: string): hwnd;
  function ForceForegroundWindow(wnd: THandle): Boolean;
  procedure mouseKlik(x,y:integer; button:integer=0);
  procedure mouseMoveTo(x,y:integer);
  //sets window for tracking. returns handle. and window size
  function targetWin(s: string; var x,y:integer): cardinal;
  procedure tWait(t:single);  //wait delayt.interval
  //procedure PostKeyEx32(key: Word; const shift: TShiftState; specialkey: Boolean);
  //posts windows message to target window
  procedure mouseMoveMsg(h:hwnd;x,y:integer);
  //SendInput keyboards event for usual apps
  procedure sendVKeys(const str:string;down,up:boolean);
  //SendInput keyboard events for DirectInput
  procedure sendScanKeys(const str:string;down:boolean=true;up:boolean=true);
  //checks color on interval for given time
  function WaitForCol(x:integer;y:integer;color:integer;time:integer):boolean;
  //get pixel color at x,y
  function getColorUnderMouse(x:integer;y:integer):integer;overload;
  //check color under current mouse pos
  function getColorUnderMouse:integer;overload;
  //memory search. only first 16 chars of leFind!
  function memFindString(leFind:string;backWards:boolean;startAddr: Cardinal = $00000000): cardinal;
  //searches memory for matching array of bytes
  function memFindArray(leFind:pSearchByteArray;backWards:boolean = false;startAddr: Cardinal = $00000000): cardinal;
  //check if os is x64 or x16
  function IAmIn64Bits: Boolean;
  //reverse bytes order in string hex as they are in memory
  function hexToMem(const hex:string):string;
  //read from memory
  procedure memRead(startAddr: Cardinal;bytes:cardinal; out st:tmemoryStream);

var
  //when true then botowe_go will be set true on each twait
  auto_go_onWait:boolean;
  botowe_go:boolean;
  appPath:string;
  //checkc if system is x64 on initialization
  is64bits:boolean;

implementation
uses mega_editor_form;
//,SendInputHelper;

//private vars
var
  waitflag:boolean;
  DelayT:TTimer;
  EvHandler:TEventHandlers;
///////////////////////////////////implementation///////////////////////

function hexToMem(const hex:string):string;
var
 i:integer;
 s:string;
begin
  if length(hex) mod 2 <>0 then begin
    log('hexToMem:: string must be dividable by 2 in length');
    result:=hex;
    exit;
  end;
  for i:=(length(hex) div 2) downto 1 do begin
    s:=hex[(i*2)-1];
    s:=s+hex[i*2];
    result:=result+(hex[(i*2)-1]+hex[i*2]);
  end;
end;

function WaitForCol(x:integer;y:integer;color:integer;time:integer):boolean;
var
  i:integer;
  pt:tpoint;
  pix:integer;
  h:hwnd;
begin
  result:=true;
  i:=-1;
  repeat
    SetCursorPos(x, y); //klik naapplication.processmessages;
    try
      h:= GetWindowDC(GetDesktopWindow);
      pix   := GetPixel(H, X, Y);
    finally
      ReleaseDC(GetDesktopWindow, h);
    end;
  twait(100);
  if time>-1 then inc(i);
  if pix=color then exit;
  until (botowe_go= false ) or (i>time);
  result:=false;
end;

function getColorUnderMouse(x:integer;y:integer):integer;
var i:integer;
  pt:tpoint;
  pix:integer;
  h:hwnd;
begin
result:=-1;
SetCursorPos(x, y); //klik naapplication.processmessages;
  try
    h := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(h, X, Y);
  finally
    ReleaseDC(GetDesktopWindow, h);
  end;
result:=pix;
end;

function getColorUnderMouse:integer;
var i:integer;
  pt:tpoint;
  pix:integer;
  h:hwnd;
begin
result:=-1;
getCursorPos(pt); //klik naapplication.processmessages;
  try
    h := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(h, pt.X, pt.Y);
  finally
    ReleaseDC(GetDesktopWindow, h);
  end;
result:=pix;
end;


procedure mouseMoveMsg(h:hwnd;x,y:integer);
begin
  postmessage(h, WM_MOUSEMOVE, 0, MakeLong(x,y));
end;

procedure sendScanKeys(const str:string;down:boolean=true;up:boolean=true);
var
  Inp: TInput;
  I,j: Integer;
begin
  try
  for I := 1 to Length(Str) do
  begin
    // press
    Inp.Itype := INPUT_KEYBOARD;
    Inp.ki.wVk :=0;
    j:=ord(uppercase(str[i])[1]);
    if j=47 then j:=189; // '/' key
    inp.ki.wScan:=VKToDIK(j);
    Inp.ki.dwFlags :=KEYEVENTF_SCANCODE;
    Inp.ki.time:=0;
    Inp.ki.dwExtraInfo:=0;
    if down then SendInput(1, Inp, SizeOf(Inp));
    sleep(20);
//  //   release
//
    Inp.ki.dwFlags := KEYEVENTF_SCANCODE or KEYEVENTF_KEYUP;
    if up then SendInput(1, Inp, SizeOf(Inp));

    Application.ProcessMessages;
    Sleep(10);
  end;
  except

  end;
end;

procedure sendVKeys(const str:string;down,up:boolean);
var
  Inp: TInput;
  I: Integer;
begin
  try
  for I := 1 to Length(Str) do
  begin
    // press

    Inp.Itype := INPUT_KEYBOARD;
    Inp.ki.wVk := ord(UpCase(str[i]));
    inp.ki.wScan:=0;
    Inp.ki.dwFlags :=0;
    Inp.ki.time:=0;
    Inp.ki.dwExtraInfo:=0;
    if down then SendInput(1, Inp, SizeOf(Inp));
    sleep(10);
  //release
    Inp.ki.dwFlags := KEYEVENTF_KEYUP;
    if up then SendInput(1, Inp, SizeOf(Inp));
    Application.ProcessMessages;
    Sleep(10);
  end;
  except

  end;
end;

procedure tWait(t:single); //wait delayt.interval
begin
  if auto_go_onWait then botowe_go:=true;
  waitflag:=true;
  if t=0 then DelayT.Interval:=1 else DelayT.Interval:=round(t);
  delayt.enabled:=true;
  repeat
    application.ProcessMessages;
  until waitflag=false;
end;

procedure mouseMoveTo(x,y:integer);
var
  Inp: TInput;
  ScreenSvc: IFMXScreenService;
  Size: TPointF;
begin
try
 if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  Size := ScreenSvc.GetScreenSize;
  Inp.Itype := INPUT_MOUSE;
  Inp.mi.dwFlags :=  MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
  Inp.mi.dx := round(x * (65535 / size.X));
  Inp.mi.dy := round(y * (65535 / size.y));
  Inp.mi.time := 0;
  Inp.mi.dwExtraInfo := 0;
  SendInput(1, Inp, SizeOf(Inp));
  except

  end;
end;

procedure mouseKlik(x,y:integer; button:integer=0);
var
  Inp: TInput;
  ScreenSvc: IFMXScreenService;
  Size: TPointF;
  flag:shortint;
begin
try
 if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  Size := ScreenSvc.GetScreenSize;

  if button = 0 then flag:=MOUSEEVENTF_LEFTDOWN;
  if button = 1 then flag:=MOUSEEVENTF_RIGHTDOWN;
  if button = 2 then flag:=MOUSEEVENTF_MIDDLEDOWN;
  Inp.Itype := INPUT_MOUSE;
  Inp.mi.dwFlags :=  flag;
  Inp.mi.dx := round(x * (65535 / size.X));
  Inp.mi.dy := round(y * (65535 / size.y));
  Inp.mi.time := 0;
  Inp.mi.dwExtraInfo := 0;
  SendInput(1, Inp, SizeOf(Inp));

  if button = 0 then flag:=MOUSEEVENTF_LEFTUP;
  if button = 1 then flag:=MOUSEEVENTF_RIGHTUP;
  if button = 2 then flag:=MOUSEEVENTF_MIDDLEUP;
  twait(20);
//  Inp.mi.time := random(20);
  Inp.mi.dwFlags :=  flag;
  SendInput(1, Inp, SizeOf(Inp));
  except

  end;
end;

function targetWin(s: string;var x,y:integer): cardinal;
var
  h:cardinal;
  WindowRect: TRect;
  ScreenSvc: IFMXScreenService;
  Size: TPointF;
begin
  size.X:=0;
  size.y:=0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
     Size := ScreenSvc.GetScreenSize;

     windowName:=s;
     h:=findWindowByName(s);
     windowHandle:=h;
     if h<>0 then begin
        GetWindowRect(h, WindowRect);
        windowPosition.X:=windowRect.Left+GetSystemMetrics(SM_CXSIZEFRAME );
        windowPosition.Y:=windowRect.top+GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYSIZEFRAME ) ;
        //try to check if window is fullscreen
        if size.X>0 then begin
           if (round(size.x)=windowRect.Width) and
              (round(size.y)=windowRect.height) then begin
                windowPosition.x:=0;
                windowPosition.y:=0;
              end;
        end;
    //    ForceForegroundWindow(h);
        x:=(windowRect.width);
        y:=(windowRect.height);
        log('target window: "'+s+'" handle: '+inttostr(h)+' w: '+inttostr(windowRect.width)+' h: '+inttostr(windowRect.height));
     end else
        log('achtung! targetWindow:: window not found');
  result:=h;
end;

function findWindowByName(partialTitle: string): hwnd;
var
  hWndTemp: hWnd;
  iLenText: Integer;
  cTitletemp: array [0..254] of Char;
  sTitleTemp: string;
begin
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do begin
    iLenText := GetWindowText(hWndTemp, cTitletemp, 255);
    sTitleTemp := cTitletemp;
    sTitleTemp := UpperCase(copy( sTitleTemp, 1, iLenText));
    partialTitle := UpperCase(partialTitle);
    if pos( partialTitle, sTitleTemp ) <> 0 then
      Break;
    hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
  end;
  result := hWndTemp;
end;

function ForceForegroundWindow(wnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(wnd) then ShowWindow(wnd, SW_RESTORE);

  if GetForegroundWindow = wnd then Result := True
  else
  begin
    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(wnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(wnd); // IE 5.5 related hack
        SetForegroundWindow(wnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = wnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(wnd); // IE 5.5 related hack
        SetForegroundWindow(Wnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0,
          TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(wnd); // IE 5.5 related hack
      SetForegroundWindow(wnd);
    end;

    Result := (GetForegroundWindow = wnd);
  end;
end; { ForceForegroundWindow }

//procedure PostKeyEx32(key: Word; const shift: TShiftState; specialkey: Boolean);
{
Parameters :
* key : virtual keycode of the key to send. For printable keys this is simply
 the ANSI code (Ord(character)) .
* shift : state of the modifier keys. This is a set, so you can set several of these keys (shift, control, alt, mouse buttons) in tandem. The TShiftState type is declared in the Classes Unit.
* specialkey: normally this should be False. Set it to True to specify a key on the numeric keypad, for example.

Description:
Uses keybd_event to manufacture a series of key events matching the passed parameters. The events go to the control with focus. Note that for characters key is always the upper-case version of the character. Sending without any modifier keys will result in a lower-case character, sending it with [ ssShift ] will result in an upper-case character!
// Simulate PRINTSCREEN - snapshot of the full screen
PostKeyEx32(VK_SNAPSHOT, [], False) ;

// Simulate PRINTSCREEN - snapshot of the active window
PostKeyEx32(VK_SNAPSHOT, [ssAlt], False) ;

// Simulate left Windows key
PostKeyEx32(VK_LWIN, [], False) ;

// Simulate Alt+F4 - close active window
PostKeyEx32(VK_F4, [ssAlt], False) ;
}{
type
   TShiftKeyInfo = record
     shift: Byte ;
     vkey: Byte ;
   end;
   ByteSet = set of 0..7 ;
const
   shiftkeys: array [1..3] of TShiftKeyInfo =
     ((shift: Ord(ssCtrl) ; vkey: VK_CONTROL),
     (shift: Ord(ssShift) ; vkey: VK_SHIFT),
     (shift: Ord(ssAlt) ; vkey: VK_MENU)) ;
var
   flag: DWORD;
   bShift: ByteSet absolute shift;
   j: Integer;
begin
   for j := 1 to 3 do
   begin
     if shiftkeys[j].shift in bShift then
       keybd_event(shiftkeys[j].vkey, MapVirtualKey(shiftkeys[j].vkey, 0), 0, 0) ;
   end;
   if specialkey then
     flag := KEYEVENTF_EXTENDEDKEY
   else
     flag := 0;

   keybd_event(key, MapvirtualKey(key, 0), flag, 0) ;
   flag := flag or KEYEVENTF_KEYUP;
   keybd_event(key, MapvirtualKey(key, 0), flag, 0) ;

   for j := 3 downto 1 do
   begin
     if shiftkeys[j].shift in bShift then
       keybd_event(shiftkeys[j].vkey, MapVirtualKey(shiftkeys[j].vkey, 0), KEYEVENTF_KEYUP, 0) ;
   end;
end;   }

{ TEventHandlers }

procedure DelayTTimer(Sender: TObject);
begin
  try
  waitflag:=false;
  delayt.Enabled:=false;
  application.ProcessMessages;
except
//sout('sleep error.. please restart the bot');
end;
end;

function IAmIn64Bits: Boolean;
var
  HandleTo64BitsProcess: WinIsWow64;
  Iret                 : BOOL;
begin
  Result := False;
  HandleTo64BitsProcess := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
  if Assigned(HandleTo64BitsProcess) then
  begin
    if not HandleTo64BitsProcess(GetCurrentProcess, Iret) then
    Raise Exception.Create('Invalid handle');
    Result := Iret;
  end;
end;

function GetModuleBaseAddress(ProcessID: Cardinal; MName: String): Pointer;
var
  Modules         : Array of HMODULE;
  cbNeeded, i     : Cardinal;
  ModuleInfo      : TModuleInfo;
  ModuleName      : Array[0..MAX_PATH] of Char;
  PHandle         : THandle;
begin
  Result := nil;
  SetLength(Modules, 1024);
  PHandle := OpenProcess(PROCESS_QUERY_INFORMATION + PROCESS_VM_READ, False, ProcessID);
  if (PHandle <> 0) then
  begin
    EnumProcessModules(PHandle, @Modules[0], 1024 * SizeOf(HMODULE), cbNeeded); //Getting the enumeration of modules
    SetLength(Modules, cbNeeded div SizeOf(HMODULE)); //Setting the number of modules
    for i := 0 to Length(Modules) - 1 do //Start the loop
    begin
      GetModuleBaseName(PHandle, Modules[i], ModuleName, SizeOf(ModuleName)); //Getting the name of module
      if AnsiCompareText(MName, ModuleName) = 0 then //If the module name matches with the name of module we are looking for...
      begin
        GetModuleInformation(PHandle, Modules[i], @ModuleInfo, SizeOf(ModuleInfo)); //Get the information of module
        Result := ModuleInfo.lpBaseOfDll; //Return the information we want (The image base address)
        CloseHandle(PHandle);
        Exit;
      end;
    end;
  end;
end;

function GetModuleBase(hProcID: Cardinal; lpModName: PChar):Cardinal;
var
  hSnap: Cardinal;
  tm: TModuleEntry32;
  flag:boolean;
begin
  result := 0;
  hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPALL , hProcID);
  if hSnap <> 0 then
  begin
    tm.dwSize := sizeof(TModuleEntry32);
    flag:= Module32First(hSnap, tm);
    if flag = true then
    begin
      while flag = true do
      begin
        if lstrcmpi(tm.szModule, lpModName) = 0 then
        begin
          result := Cardinal(tm.modBaseAddr);
          break;
        end;
      flag:=Module32Next(hSnap, tm);
      end;
    end;
    CloseHandle(hSnap);
  end;
end;

function memFindString(leFind:string;backWards:boolean;startAddr: Cardinal = $00000000): cardinal;
 var
   ProcId: Cardinal;
   tProc: THandle;
   BUFFMAX,bytesRead: NativeUInt;
   i,j,searchLength:cardinal;
   pos:Cardinal;
   matchindex: byte;       // licznik pasujacych kolejnych bajtow
   target: array[0..15] of Byte;
   Memblock : array[0..2048] of Byte;
   memEnd : cardinal;
   b:boolean;
begin
  if windowHandle=0 then begin
    log('memFindString:: targetWindow is not set');
    result:=0;
    exit;
  end;
  BUFFMAX := 2048;
  for I := 0 to length(target) do begin
    target[i]:=integer(leFind[i+1]);
    if target[i]=0 then break;
  end;
  searchLength:=i;
  pos:=startAddr;
  if is64bits then memEnd:= $7fffffffffffffff else memEnd:=$ffffffff;
  result:=0;
  GetWindowThreadProcessId(windowHandle, @ProcId) ;
  tProc:= OpenProcess(PROCESS_VM_READ , False, ProcId); //otwarcie pamieci
  matchindex:=0; // licznik pasujacych kolejnych bajtow

  while (pos <= memEnd-BUFFMAX) and (result=0) do
      begin
        b:=ReadProcessMemory(tProc, Ptr(pos), @memblock, buffmax, bytesread); //pobranie wartosc z adresu
      //  if not b then OutputDebugString(pwidechar('read memory failed: '+inttostr(GetLastError)));
        for i:= 0 to buffmax-1 do
        begin
          if memblock[i]= target[matchindex] then
              begin
              if matchindex >= searchLength-1 then begin
                result:=pos+i-matchindex;
                break;
               end;
              inc(matchIndex);
              end
              else matchindex:=0;
        end;
        if not backWards then pos:=pos+buffmax else pos:=pos-buffmax;
      end;
     CloseHandle(tProc);
  //clear buffers and shit
  for I := 0 to length(target) do target[i]:=0;
  for I := 0 to BUFFMAX do Memblock[i]:=0;
end;

function memFindArray(leFind:pSearchByteArray;backWards:boolean = false;startAddr: Cardinal = $00000000): cardinal;
var
   ProcId: Cardinal;
   tProc: THandle;
   BUFFMAX,bytesRead: NativeUInt;
   i,j,searchLength:cardinal;
   pos:Cardinal;
   matchindex: byte;       // licznik pasujacych kolejnych bajtow
   Memblock : array[0..16383] of Byte;
   memEnd : cardinal;
   b:boolean;
   size:integer;//search patter length
   pmc: PPROCESS_MEMORY_COUNTERS;
   cb: Integer;
   memBase:^integer;
begin
  result:=0;
  if windowHandle=0 then begin
    log('memFindArray:: targetWindow hwnd is not set');
    exit;
  end;
  size:=length(leFind^);
  BUFFMAX := 16383;
  if size>buffmax then begin
    log('memFindArray:: search patter too long. max size: '+inttostr(buffmax));
    exit;
  end;
  searchLength:=size;
  pos:=startAddr;
  if is64bits then memEnd:= $7fffffffffffffff else memEnd:=$ffffffff;
  try
    GetWindowThreadProcessId(windowHandle, @ProcId) ;
    tProc:= OpenProcess(PROCESS_QUERY_INFORMATION  or PROCESS_VM_READ , False, ProcId); //otwarcie pamieci
    //try to get memory range used by exe
    if windowName<>'' then begin
      //get memory size used by process
      cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
      GetMem(pmc, cb);
      pmc^.cb := cb;
      if pos=0 then pos:=GetModuleBase(ProcId, pwidechar(windowName+'.exe'))+startAddr;
      if GetProcessMemoryInfo(tProc, pmc, cb) then
         memEnd:=(pmc^.WorkingSetSize+pos)
      else
        log('Unable to get process info');
      FreeMem(pmc);
    end;
    matchindex:=0; // licznik pasujacych kolejnych bajtow
    while (pos <= memEnd-BUFFMAX) and (result=0) do
      begin
        b:=ReadProcessMemory(tProc, Ptr(pos), @memblock, buffmax, bytesread); //pobranie wartosc z adresu
      //  if not b then OutputDebugString(pwidechar('read memory failed: '+inttostr(GetLastError)));
        for i:= 0 to buffmax-1 do
        begin
          if memblock[i]= leFind^[matchindex] then begin
              if matchindex >= searchLength-1 then begin
                 result:=pos+i-matchindex;
                 break;
              end;
              inc(matchIndex);
          end
          else matchindex:=0;
        end;
        if not backWards then pos:=pos+buffmax else pos:=pos-buffmax;
      end;
      if pos >= memEnd-BUFFMAX then result:=0;
  finally
   CloseHandle(tProc);
  end;
end;

procedure memRead(startAddr: Cardinal;bytes:cardinal; out st:tmemoryStream);
var
   ProcId: Cardinal;
   tProc: THandle;
   BUFFMAX,bytesRead: NativeUInt;
   pos,i:Cardinal;
   Memblock : array[0..2048] of Byte;
   current:string;
   memEnd ,error: cardinal;
   bytesToRead:int64;
   b:boolean;
begin
  if windowHandle=0 then log('memRead:: targetWindow hwnd is not set');
  BUFFMAX := 2048;
  pos:=startAddr;
  if is64bits then memEnd:= $7fffffffffffffff else memEnd:=$ffffffff;
  st.Seek(0,soFromBeginning);
  try
    GetWindowThreadProcessId(windowHandle, @ProcId) ;
    tProc:= OpenProcess(PROCESS_VM_READ , False, ProcId); //otwarcie pamieci
    bytesToRead:=bytes;
    while (bytesToRead>0) do
    begin
       b:=ReadProcessMemory(tProc, Ptr(pos), @memblock, BUFFMAX, bytesread); //pobranie wartosc z adresu
       //if not b then OutputDebugString(pwidechar('read memory failed: '+inttostr(GetLastError)));
       inc(pos,buffmax);
       if bytesToRead>buffmax then begin
         st.WriteBuffer(memblock,buffmax);
         dec(bytesToRead,buffmax);
       end else
       begin
         st.WriteBuffer(memblock,bytesToRead);
         break;
       end;
    end;
  finally
    CloseHandle(tProc);
    st.Seek(0,soFromBeginning)
  end;
end;

{memory sarch 'example'
procedure TformEditor.Button1Click(Sender: TObject);
var
 search:TSearchByteArray;
 s,s1,biote:string;
 i:integer;
begin
  s:='Laggy Luk';
  i:=31765689;
  biote:=hexToMem(inttohex(i,8));
  setlength(search,8+length(s));
  for i:=0 to (length(biote) div 2)-1 do begin
      s1:='$'+copy(biote,i*2+1,2);
      search[i]:= strtoint(s1);
  end;
  for i:=1 to length(s) do search[4+i-1]:=integer(s[i]);
  log(inttostr(memFindArray(@search,false)));
end;
}
procedure TEventHandlers.ontime(Sender: TObject);
begin
  DelayTTimer(nil);
end;

initialization
  DelayT:=ttimer.Create(nil);
  DelayT.OnTimer:=evhandler.ontime;
  auto_go_onWait:=true;
  appPath:=extractfilepath(ParamStr(0));
  is64bits:= IAmIn64Bits;
finalization
  delayt.Free;
end.


