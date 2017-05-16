unit meta_botowe_win;

interface

uses controls,windows,messages,shellapi,forms,clipbrd,SysUtils,
     dialogs,ComCtrls,ExtCtrls,stdctrls,grids,Graphics,classes,inifiles;

   procedure pasteLink(s:string); //pastes link to ffwindow but by xy so becareful
   procedure pasteWord(s:string);
   procedure reconnect;
   procedure runBatandWait(f:string);  //runs a batch file
   procedure runBat(f:string);  //runs a batch file
   procedure clickat(x,y: integer);
//   procedure clickatRnd(x,y,range: integer);
   function clickAtHand(x,y: integer):integer;
   function typeWord(str: string): string;
   procedure scrollDown(c:integer);
   procedure pageDown(c:integer);
   function randomWord(c:integer):string;
   procedure wheelUp(x,y:integer;count:integer);
   procedure clickatRndT(x,y,range: integer);
   procedure clickatRnd(x,y:integer;range: integer=5);overload;
   procedure clickatRnd(xy: tpoint;rndRange:integer=5);overload;
   procedure pressBackspace(c:integer);
   procedure ctrlTab;
   procedure ctrlA;
   procedure keyDown;
   procedure esc;
   procedure pressEnter;
   procedure pressDel;
   procedure pressEnd;
   procedure pressTab;
   function GetVersionExeName(aFile: String): single;
   procedure PostKeyEx32(key: Word; const shift: TShiftState; specialkey: Boolean) ;
   procedure closeFF;
   function eatString(var s: string):string;
   function IsNumber(const inStr: string): Boolean;
   function ForceForegroundWindow(wnd: THandle): Boolean;
   function CopySelectedText:string;
   function ClearString(Str: string): string;
   procedure wait(t:integer;var flag:boolean);
   procedure closeTab(count:integer);
   function Parsing(Char, Str: string; Count: Integer): string;
   function parse(com:string):boolean;
   //parses list of commands and returns -1 if ok, else number of errours line
   function parseScript(scr:tstringList):integer;  overload;
   function parseScript(lePath:string):integer; overload;
   function WaitForColor(x:integer;y:integer;color:string;time:integer):boolean;overload;
   function WaitForColor(x:integer;y:integer;color:integer;time:integer):boolean;overload;
   function checkColor(x:integer;y:integer;color:integer):boolean;overload;
   function checkColor(xy:tpoint;color:integer):boolean;overload;
   function getColor(x:integer;y:integer):integer;
   function tWait(t:single):boolean; //wait delayt.interval
   procedure DelayTTimer(Sender: TObject);
   function resizeWindowByName(windowName:string;x,y,w,h:integer):boolean;
   function closeWindowByHwnd(h:Hwnd): boolean;
   function closeWindowByName(name:string): boolean;
   function closeWindowByClass(name:string): boolean;
   function minimizeWindowByName(name:string): boolean;
   function activateWindowByName(name: string): boolean;
   //round float to given number of digits
   function Rounder(var Value: Double; Decimals: Integer): Double;
   Function PosEx(SubStr, Str: String; PosStart: Integer): Integer;
   procedure SmoothResize(abmp:TBitmap; NuWidth,NuHeight:integer);
   procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;specialkey: Boolean);
   function memFindString(leFind:string;backWards:boolean;startAddr: Cardinal = $00000000): cardinal;
   procedure memRead(startAddr: Cardinal;bytes:cardinal; out st:tmemoryStream);
   //take screenshot of active window or entire desktop
   procedure ScreenShot(activeWindow: bool; destBitmap : TBitmap) ;
   // function memFindString(endAddr:cardinal;startAddr: Cardinal = $00400000): cardinal; //Read adress:value
   procedure ScreenShotActiveWindow(Bild: TBitMap);
   //find window by part of it's name
   function FindWindowExtd(partialTitle: string): HWND;
   { Returns a count of the number of occurences of SubText in Text }
   function CountOccurences( const SubText: string;const Text: string): Integer;
   procedure saveControl(obj:tcontrol);
   procedure loadControl(obj:tcontrol);
   procedure muteSound; //mute by sending volume down to mixer..
   procedure unMuteSound;
   function GetModuleName: string;//returns path to dll location. like appexename
   procedure SaveStringGrid(StringGrid: TStringGrid; const FileName: TFileName);
   procedure LoadStringGrid(StringGrid: TStringGrid; const FileName: TFileName);
type
   TEventHandlers = class // create a dummy class
       procedure ontime(Sender: TObject) ;
   end;

   TRGBArray = ARRAY[0..32767] OF TRGBTriple;
   pRGBArray = ^TRGBArray;

const
   //max range memory will be searched, perhaps should be bigger for 64bit
   memEndAddr =$ffffffff;

var waitflag:boolean;
    DelayT:TTimer;
    botowe_go:boolean;
    auto_go_onWait:boolean;//when true then botowe_go will be set true on each twait
    EvHandler:TEventHandlers;
    appPath:string;

implementation

function GetModuleName: string;
var
  szFileName: array[0..MAX_PATH] of Char;
begin
  FillChar(szFileName, SizeOf(szFileName), #0);
  GetModuleFileName(hInstance, szFileName, MAX_PATH);
  Result := szFileName;
end;

procedure PostKeyEx32(key: Word; const shift: TShiftState; specialkey: Boolean) ;
{
Parameters :
* key : virtual keycode of the key to send. For printable keys this is simply the ANSI code (Ord(character)) .
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
}
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
end;

procedure saveControl(obj:tcontrol);
var
  ini:tinifile;
  edit:tedit;
  up:tupdown;
  le:tlabelededit;
  ch:tcheckbox;
begin
  ini:=tinifile.Create(appPath+'config.ini');
  if obj is tedit then  begin
    edit:= obj as tedit;
    ini.WriteString('options',edit.name,edit.text);
  end;
  if obj is tlabelededit then  begin
    le:= obj as tlabelededit;
    ini.WriteString('options',le.name,le.Text);
  end;
  if obj is tupdown then  begin
    up:= obj as tupdown;
    ini.WriteInteger('options',up.name,up.position);
  end;
  if obj is TCheckBox then  begin
    ch:= obj as TCheckbox;
    ini.writebool('options',ch.name,ch.checked);
  end;
  ini.Free;
end;

procedure loadControl(obj:tcontrol);
var
  ini:tinifile;
  edit:tedit;
  le:tlabelededit;
  up:tupdown;
  s:string;
  ch:tcheckbox;
  i:integer;
begin
  ini:=tinifile.Create(appPath+'config.ini');
  if obj is tedit then  begin
    edit:= obj as tedit;
    s:=ini.ReadString('options',edit.name,'');
    if s<>'' then edit.Text:=s;
  end;
  if obj is tlabelededit then  begin
    le:= obj as tlabelededit;
    s:=ini.ReadString('options',le.name,'');
    if s<>'' then le.Text:=s;
  end;
  if obj is tupdown then  begin
    up:= obj as tupdown;
    i:=ini.ReadInteger('options',up.name,0);
    up.position:=i;
  end;
  if obj is TCheckBox then  begin
    ch:= obj as TCheckbox;
    ch.checked:=ini.Readbool('options',ch.name,false);
  end;
  ini.Free;
end;

// Save a TStringGrid to a file

procedure SaveStringGrid(StringGrid: TStringGrid; const FileName: TFileName);
var
  f:    TextFile;
  i, k: Integer;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  with StringGrid do
  begin
    // Write number of Columns/Rows
    Writeln(f, ColCount);
    Writeln(f, RowCount);
    // loop through cells
    for i := 0 to ColCount - 1 do
      for k := 0 to RowCount - 1 do
        Writeln(F, Cells[i, k]);
  end;
  CloseFile(F);
end;

// Load a TStringGrid from a file

procedure LoadStringGrid(StringGrid: TStringGrid; const FileName: TFileName);
var
  f:          TextFile;
  iTmp, i, k: Integer;
  strTemp:    String;
begin
  AssignFile(f, FileName);
  Reset(f);
  with StringGrid do
  begin
    // Get number of columns
    Readln(f, iTmp);
    ColCount := iTmp;
    // Get number of rows
    Readln(f, iTmp);
    RowCount := iTmp;
    // loop through cells & fill in values
    for i := 0 to ColCount - 1 do
      for k := 0 to RowCount - 1 do
      begin
        Readln(f, strTemp);
        Cells[i, k] := strTemp;
      end;
  end;
  CloseFile(f);
end;

function eatString(var s: string):string;
var i:integer;
begin
  result:='';
  if pos(';',s)=0 then exit;
  for I := 1 to length(s) do if s[i]=';' then break;
  result:=copy(s,1,i-1);
  s:=copy(s,i+1,length(s)-i);
end;

{ Returns a count of the number of occurences of SubText in Text }
function CountOccurences( const SubText: string;const Text: string): Integer;
begin
  if (SubText = '') OR (Text = '') OR (Pos(SubText, Text) = 0) then
    Result := 0
  else
    Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div  Length(subtext);
end;  { CountOccurences }


function FindWindowExtd(partialTitle: string): HWND;
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

 procedure ScreenShot(activeWindow: bool; destBitmap : TBitmap) ;
 var
    w,h : integer;
    DC : HDC;
    hWin : Cardinal;
    r : TRect;
 begin
    if activeWindow then
    begin
      hWin := GetForegroundWindow;
      dc := GetWindowDC(hWin) ;
      GetWindowRect(hWin,r) ;
      w := r.Right - r.Left;
      h := r.Bottom - r.Top;
    end
    else
    begin
      hWin := GetDesktopWindow;
      dc := GetDC(hWin) ;
      w := GetDeviceCaps (DC, HORZRES) ;
      h := GetDeviceCaps (DC, VERTRES) ;
    end;

    try
     destBitmap.Width := w;
     destBitmap.Height := h;
     BitBlt(destBitmap.Canvas.Handle,
            0,
            0,
            destBitmap.Width,
            destBitmap.Height,
            DC,
            0,
            0,
            SRCCOPY) ;
    finally
     ReleaseDC(hWin, DC) ;
    end;
 end;

procedure ScreenShotActiveWindow(Bild: TBitMap);
var
  c: TCanvas;
  r, t: TRect;
  h: THandle;
begin
  c := TCanvas.Create;
  c.Handle := GetWindowDC(GetDesktopWindow);
  h := GetForeGroundWindow;
  if h <> 0 then
    GetWindowRect(h, t);
  try
    r := Rect(0, 0, t.Right - t.Left, t.Bottom - t.Top);
    Bild.Width  := t.Right - t.Left;
    Bild.Height := t.Bottom - t.Top;
    Bild.Canvas.CopyRect(r, c, t);
  finally
    ReleaseDC(0, c.Handle);
    c.Free;
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
BUFFMAX := 2048;
for I := 0 to length(target) do begin
  target[i]:=integer(leFind[i+1]);
  if target[i]=0 then break;
end;
searchLength:=i;
pos:=startAddr;
memEnd:= memEndAddr;
result:=0;
GetWindowThreadProcessId(application.Handle, @ProcId) ;
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
BUFFMAX := 2048;
pos:=startAddr;
memEnd:= memEndAddr;
GetWindowThreadProcessId(application.Handle, @ProcId) ;
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
CloseHandle(tProc);
for I := 0 to BUFFMAX do Memblock[i]:=0;
end;

function memScan(endAddr:cardinal;startAddr: Cardinal = $00400000): string; //Read adress:value
 const memStart = $00400000;

 var
   ProcId: Cardinal;
   tProc: THandle;
   BUFFMAX,bytesRead: NativeUInt;
   i,j:integer;
   pos:Cardinal;
   matchindex: byte;       // licznik pasujacych kolejnych bajtow
   target: array[0..15] of Byte;
   Memblock : array[0..65536 - 1] of Byte;
   hits: array of string;
   current:string;
//   handla: HWND;
   memEnd : cardinal;
begin
BUFFMAX := 65536;
//18 00 00 00 73 B8 9C 71 00 00 00 00 7C FC FF FF
target[0]:= $18;
target[1]:= $00;
target[2]:= $00;
target[3]:= $00;
target[4]:= $73;
target[5]:= $b8;
target[6]:= $9c;
target[7]:= $71;
target[8]:= $00;
target[9]:= $00;
target[10]:= $00;
target[11]:= $00;
target[12]:= $7c;
target[13]:= $fc;
target[14]:= $ff;
target[15]:= $ff;
pos:=startAddr;
memEnd:= endAddr;
result:='code not found';
GetWindowThreadProcessId(FindWindow(Nil,'aaa'), @ProcId) ;
tProc:= OpenProcess(PROCESS_ALL_ACCESS, False, ProcId); //otwarcie pamieci
matchindex:=0; // licznik pasujacych kolejnych bajtow
setlength(hits,0);
while pos <= memEnd-BUFFMAX do
    begin
      ReadProcessMemory(tProc, Ptr(pos), @memblock, buffmax, bytesread); //pobranie wartosc z adresu
      for i:= 0 to buffmax-1 do
      begin
        if memblock[i]= target[matchindex] then
            begin
            if matchindex >= length(target)-1 then begin
              current:=char(memblock[i+1])+char(memblock[i+3])+char(memblock[i+5])+char(memblock[i+7]) ;
       //     showmessage(current+' found at: '+inttostr(pos+i));
              setlength(hits,length(hits)+1);
              hits[High(hits)]:=current;
              matchindex:=0;
             end;
            inc(matchIndex);
            end
            else matchindex:=0;
      end;
      pos:=pos+buffmax;
    end;
   CloseHandle(tProc);
end;

procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;
  specialkey: Boolean);
type
  TBuffers = array [0..1] of TKeyboardState;
var
  pKeyBuffers: ^TBuffers;
  lParam: LongInt;
begin
  if IsWindow(hWindow) then
  begin
    pKeyBuffers := nil;
    lParam := MakeLong(0, MapVirtualKey(key, 0));
    if specialkey then
      lParam := lParam or $1000000;
    New(pKeyBuffers);
    try
      (* Fill buffer 1 with current state so we can later restore it.
         Null out buffer 0 to get a "no key pressed" state. *)
      GetKeyboardState(pKeyBuffers^[1]);
      FillChar(pKeyBuffers^[0], SizeOf(TKeyboardState), 0);

      (* set the requested modifier keys to "down" state in the buffer*)
      if ssShift in shift then
        pKeyBuffers^[0][VK_SHIFT] := $80;
      if ssAlt in shift then
      begin
        (* Alt needs special treatment since a bit in lparam needs also be set *)
        pKeyBuffers^[0][VK_MENU] := $80;
        lParam := lParam or $20000000;
      end;
      if ssCtrl in shift then
        pKeyBuffers^[0][VK_CONTROL] := $80;
      if ssLeft in shift then
        pKeyBuffers^[0][VK_LBUTTON] := $80;
      if ssRight in shift then
        pKeyBuffers^[0][VK_RBUTTON] := $80;
      if ssMiddle in shift then
        pKeyBuffers^[0][VK_MBUTTON] := $80;
       (* make out new key state array the active key state map *)
      SetKeyboardState(pKeyBuffers^[0]);
      (* post the key messages *)
      if ssAlt in Shift then
      begin
        PostMessage(hWindow, WM_SYSKEYDOWN, key, lParam);
        PostMessage(hWindow, WM_SYSKEYUP, key, lParam or $C0000000);
      end
      else
      begin
        PostMessage(hWindow, WM_KEYDOWN, key, lParam);
        PostMessage(hWindow, WM_KEYUP, key, lParam or $C0000000);
      end;
      (* process the messages *)
      Application.ProcessMessages;
       (* restore the old key state map *)
      SetKeyboardState(pKeyBuffers^[1]);
    finally
      (* free the memory for the key state buffers *)
      if pKeyBuffers <> nil then
        Dispose(pKeyBuffers);
    end; { If }
  end;
end;

procedure SmoothResize(abmp:TBitmap; NuWidth,NuHeight:integer);
var
  weight_x, weight_y : array[0..1] of Single;
  ifrom_y, ifrom_x : Integer;
  to_y, to_x : Integer;
  new_red, new_green : Integer;
  new_blue : Integer;
  ix, iy : Integer;
  xscale, yscale : Single;
  sfrom_y, sfrom_x : Single;
  weight : Single;
  total_red, total_green : Single;
  total_blue : Single;
  bTmp : TBitmap;
  sli, slo : pRGBArray;
  // pointers for scanline access
  liPByte, loPByte, p : PByte;
  // offset increment
  liSize, loSize : integer;
begin
  try
  abmp.PixelFormat := pf24bit;
  bTmp := TBitmap.Create;
  bTmp.PixelFormat := pf24bit;
  bTmp.Width := NuWidth;
  bTmp.Height := NuHeight;
  xscale := bTmp.Width / (abmp.Width-1);
  yscale := bTmp.Height / (abmp.Height-1);
  liPByte := abmp.Scanline[0];
  liSize := integer(abmp.Scanline[1]) -integer(liPByte);
  loPByte := bTmp.Scanline[0];
  loSize := integer(bTmp.Scanline[1]) -integer(loPByte);
  for to_y := 0 to bTmp.Height-1 do begin
  sfrom_y := to_y / yscale;
  ifrom_y := Trunc(sfrom_y);
  weight_y[1] := sfrom_y - ifrom_y;
  weight_y[0] := 1 - weight_y[1];
  for to_x := 0 to bTmp.Width-1 do begin
  sfrom_x := to_x / xscale;
  ifrom_x := Trunc(sfrom_x);
  weight_x[1] := sfrom_x - ifrom_x;
  weight_x[0] := 1 - weight_x[1];
  total_red := 0.0;
  total_green := 0.0;
  total_blue := 0.0;
  for ix := 0 to 1 do begin
  for iy := 0 to 1 do begin
  p := liPByte;
  Inc(p, liSize *(ifrom_y + iy));
  sli := pRGBArray(p);
  new_red := sli[ifrom_x + ix].rgbtRed;
  new_green := sli[ifrom_x + ix].rgbtGreen;
  new_blue := sli[ifrom_x + ix].rgbtBlue;
  weight := weight_x[ix] * weight_y[iy];
  total_red := total_red + new_red * weight;
  total_green := total_green + new_green * weight;
  total_blue := total_blue + new_blue * weight;
  end;
end;
  p := loPByte;
  Inc(p, loSize *to_y);
  slo := pRGBArray(p);
  slo[to_x].rgbtRed := Round(total_red);
  slo[to_x].rgbtGreen := Round(total_green);
  slo[to_x].rgbtBlue := Round(total_blue);
  end;
  end;
  abmp.Width := bTmp.Width;
  abmp.Height := bTmp.Height;
  abmp.Canvas.Draw(0,0,bTmp);
  finally
   bTmp.Free;
  end;
end;

function ReadVersionInfo(sProgram: string;Major,Minor,Release,Build: pWord): Boolean;
var
  Info: PVSFixedFileInfo;
  InfoSize: Cardinal;
  nHwnd: DWORD;
  BufferSize: DWORD;
  Buffer: Pointer;
begin
  BufferSize := GetFileVersionInfoSize(pchar(sProgram),nHWnd); {Get buffer size}
  Result := True;
  if BufferSize <> 0 then
  begin {if zero, there is no version info}
  GetMem( Buffer, BufferSize); {allocate buffer memory}

  try
  if GetFileVersionInfo(PChar(sProgram),nHWnd,BufferSize,Buffer) then
  begin
  {got version info}
  if VerQueryValue(Buffer, '\', Pointer(Info), InfoSize) then
  begin {got root block version information}
  if Assigned(Major) then
  Major^ := HiWord(Info^.dwFileVersionMS); {extract major version}

  if Assigned(Minor) then
  Minor^ := LoWord(Info^.dwFileVersionMS); {extract minor version}

  if Assigned(Release) then
  Release^ := HiWord(Info^.dwFileVersionLS); {extract release version}

  if Assigned(Build) then
  Build^ := LoWord(Info^.dwFileVersionLS); {extract build version}
  end
  else
  Result := False; {no root block version info}
  end
  else
  Result := False; {couldn't extract version info}
  finally
    FreeMem(Buffer, BufferSize); {release buffer memory}
  end;
  end
  else
  Result := False; {no version info at all in the file}
end;

function GetVersionExeName(aFile: String): single;
var
  Major: Word;
  Minor: Word;
  Release: Word;
  Build: Word;
begin
  {get version information from file sFile}
  if ReadVersionInfo(aFile,@Major,@Minor,@Release,@Build) then
  begin
  Major := Integer(Major); {set major version property}
  Minor := Integer(Minor); {set minor version property}
  Release := Integer(Release); {set release version property}
  Build := Integer(Build); {set build version property}
  Result := strtofloat(IntToStr(Major) + '.' + IntToStr(Minor) + IntToStr(Release) + IntToStr(Build));
  end
  else
  Result := 0;
end;

Function PosEx(SubStr, Str: String; PosStart: Integer): Integer;
Begin
  if Pos(SubStr, Copy(Str, PosStart, Length(Str)-PosStart+1)) = 0 Then
    Result := 0 Else
    Result := Pos(SubStr, Copy(Str, PosStart, Length(Str)-PosStart+1))+PosStart-1;
End;
// Load a TStringGrid from a file

function IsNumber(const inStr: string): Boolean;
var
  i: extended;
begin
  Result := TryStrToFloat(inStr,i);
end;

procedure TEventHandlers.ontime(Sender: TObject) ;
begin
  DelayTTimer(nil);
end;

function GetSystemDir: TFileName;
var
  SysDir: array [0..MAX_PATH-1] of char;
begin
  SetString(Result, SysDir, GetSystemDirectory(SysDir, MAX_PATH));
  if Result = '' then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

function Rounder(var Value: Double; Decimals: Integer): Double;
var
  j: Integer;
  A: Double;
begin
  A := 1;
  case Decimals of
    0: A := 1;
    1: A := 10;
    else
      for j := 1 to Decimals do
        A := A * 10;
  end;
  Result := Int((Value * A) + 0.5) / A;
end;

function parseScript(scr:tstringlist):integer;
var i:integer;
begin
  for I := 0 to scr.Count-1 do
    begin
      twait(20);
      parse(scr[i]);
    end;
if i=scr.Count-1 then i:=-1;
result:=i;
end;

function parseScript(lePath:string):integer;
var scr:tstringlist;
begin
try
  scr:=tstringlist.Create;
  scr.LoadFromFile(lePath);
  parseScript(scr);
finally
  scr.Free;
end;
end;

// parse macro command. result true if command recognized
function parse(com:string):boolean;
var
  ts:string;
  i,int1,int2,int3,int4:integer;
  s1,s2,s3:string;
begin
if com='' then begin result:=false; exit; end;
ts:=lowercase(parsing(' ',com,1));
result:=true;
if ts ='clickat' then
   begin
   int1:=strtoint(parsing(' ',com,2));
   int2:=strtoint(parsing(' ',com,3));
   clickat(int1,int2);
   exit;
   end;

if ts ='stop' then
   begin
   result:=false;
   exit;
   end;

if ts ='clickatrnd' then
   begin
   int1:=strtoint(parsing(' ',com,2));
   int2:=strtoint(parsing(' ',com,3));
   int3:=strtoint(parsing(' ',com,4));
   clickatrnd(int1,int2,int3);
   exit;
   end;

if ts ='waitforcolor' then
   begin
   int1:=strtoint(parsing(' ',com,2));
   int2:=strtoint(parsing(' ',com,3));
   s1:=parsing(' ',com,4);
   int3:=strtoint(parsing(' ',com,5));
   if not waitforcolor(int1,int2,s1,int3) then result:=false;
   exit;
   end;

   // sleep r fromMs toMs
   //sleep ms
if ts ='sleep' then
   begin
   s1:=parsing(' ',com,2);
   if s1='r' then
   begin
     int1:=strtoint(parsing(' ',com,3));
     int2:=strtoint(parsing(' ',com,4));
     twait(random(int2-int1)+int1);
     exit;
   end;
   // else nie potrzebne bo jest exit
   int1:=strtoint(parsing(' ',com,2));
   twait(int1);
   exit;
   end;

if ts ='typeword' then
   begin
   s1:=(parsing(' ',com,2));
   typeword(s1);
   exit;
   end;

if ts ='keyctrla' then
   begin
   ctrlA;
   exit;
   end;

if ts ='keyend' then
   begin
   pressEnd;
   exit;
   end;

if ts ='keydown' then
   begin
   keyDown;
   exit;
   end;

if ts ='keyenter' then
   begin
   pressEnter;
   exit;
   end;

if ts ='keytab' then
   begin
   pressTab;
   exit;
   end;

if ts ='keyesc' then
   begin
   esc;
   exit;
   end;

if (ts[1] ='/') and (ts[2] ='/') then
   begin
   exit;
   end;

if ts ='navigate' then
   begin
   s1:=(parsing(' ',com,2));
   pastelink(s1);
   exit;
   end;

if ts ='closetab' then
   begin
   closetab(1);
   exit;
   end;

if ts ='clickatrndt' then
   begin
   int1:=strtoint(parsing(' ',com,2));
   int2:=strtoint(parsing(' ',com,3));
   int3:=strtoint(parsing(' ',com,4));
   clickatrndt(int1,int2,int3);
   exit;
   end;

//podajemy kwadrat i ile sie w nim miesci 'jednostek'
//   rect i dzielnik
if ts ='clickatrange' then
   begin
   int1:=strtoint(parsing(' ',com,2));
   int2:=strtoint(parsing(' ',com,3));
   int3:=strtoint(parsing(' ',com,4));
   int4:=strtoint(parsing(' ',com,5));

   //deal jest taki ze obliczamy ile ma odstep w pixelach pomiedzy dniami
  int3:= (int3-int1) div 6;
  int4:= (int4-int2) div 2;
//   sout(inttostr(int3));
//      sout(inttostr(int4));
   // i klikamy w losowy w tym zakresie
   int1:=int1+random(7)*int3;
   int2:=int2+random(3)*int4;
   clickat(int1,int2);
//   sout(inttostr(int1)+ ' ' +inttostr(int2));
   exit;
   end;

if ts ='scrolld' then
   begin
   s1:=(parsing(' ',com,2));
   scrolldown(strtoint(s1));
   exit;
   end;
if ts ='reconnect' then
   begin
   s1:=(parsing(' ',com,2));
   reconnect;
   exit;
   end;
result:= false;
end;

//chyba zwraca n-ty wyraz ze Str a CHar to znak podzialu wyrazow
function Parsing(Char, Str: string; Count: Integer): string;
var
  i: Integer;
  strResult: string;
begin
if length(str)=0 then exit;

  if Str[Length(Str)] <> Char then
    Str := Str + Char;
  for i := 1 to Count do
  begin
    strResult := Copy(Str, 0, Pos(Char, Str) - 1);
    Str := Copy(Str, Pos(Char, Str) + 1, Length(Str));
  end;
  Result := strResult;
end;

// click random nuber of times
procedure clickatRndT(x,y,range: integer);
 var
    Pt : TPoint;
    i:integer;
begin
for i:=0 to random(range) do begin
SetCursorPos(x, y); //klik na
mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
sleep(100);
mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
sleep (300);
application.processmessages;
end;
//sout('clickatrnd');
end;

function resizeWindowByName(windowName:string;x,y,w,h:integer):boolean;
var
HWnd : THandle;
begin
  result:=false;
  IF FINDWINDOW(NIL,pchar(windowName))<>0 then
  begin
    HWnd := FindWindow(nil, pchar(windowName));
    //setforegroundwindow(hwnd);
    result:=SetWindowPos(hWnd,HWND_TOP,x,y,w,h,SWP_SHOWWINDOW);
  end;
end;

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

function tWait(t:single):boolean; //wait delayt.interval
begin
if auto_go_onWait then botowe_go:=true;
waitflag:=true;
if t=0 then DelayT.Interval:=1 else DelayT.Interval:=round(t);
delayt.enabled:=true;
repeat
application.ProcessMessages;
until waitflag=false;
end;

function WaitForColor(x:integer;y:integer;color:integer;time:integer):boolean;
var i:integer;
pt:tpoint;
c: TCanvas;
pix:integer;
begin
result:=true;
i:=-1;
repeat
SetCursorPos(x, y); //klik naapplication.processmessages;
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(c.Handle, X, Y);
  finally
    ReleaseDC(GetDesktopWindow, c.Handle);
    c.Free;
  end;
twait(100);
if time>-1 then inc(i);
if pix=color then exit;
until (botowe_go= false ) or (i>time);
result:=false;
end;

function WaitForColor(x:integer;y:integer;color:string;time:integer):boolean;
var i:integer;
pt:tpoint;
c: TCanvas;
pix:integer;
begin
result:=true;
i:=0;
repeat
SetCursorPos(x, y); //klik naapplication.processmessages;
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(c.Handle, X, Y);
  finally
    ReleaseDC(GetDesktopWindow, c.Handle);
    c.Free;
  end;
twait(100);
inc(i);
if pix=strtoint(color) then exit;
until (botowe_go= false ) or (i>time);
result:=false;
end;

function getColor(x:integer;y:integer):integer;
var i:integer;
  pt:tpoint;
  c: TCanvas;
  pix:integer;
begin
result:=-1;
SetCursorPos(x, y); //klik naapplication.processmessages;
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(c.Handle, X, Y);
  finally
    ReleaseDC(GetDesktopWindow, c.Handle);
    c.Free;
  end;
result:=pix;
end;

function checkColor(x:integer;y:integer;color:integer):boolean;overload;
var i:integer;
  pt:tpoint;
  c: TCanvas;
  pix:integer;
begin
result:=true;
SetCursorPos(x, y); //klik naapplication.processmessages;
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(c.Handle, X, Y);
  finally
    ReleaseDC(GetDesktopWindow, c.Handle);
    c.Free;
  end;
if pix<>color then result:=false;
end;

function checkColor(xy:tpoint;color:integer):boolean;overload;
var i:integer;
  pt:tpoint;
  c: TCanvas;
  pix:integer;
begin
result:=true;
SetCursorPos(xy.x, xy.y); //klik naapplication.processmessages;
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    pix   := GetPixel(c.Handle, xy.X, xy.Y);
  finally
    ReleaseDC(GetDesktopWindow, c.Handle);
    c.Free;
  end;
if pix<>color then result:=false;
end;


procedure closeTab(count:integer);
var i:integer;
begin
for i:=1 to count do begin
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(30);
  keybd_event(Ord('W'), 0, 0, 0);
  sleep(30);
  keybd_event(Ord('W'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(30);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(30);
  end;
end;

procedure wait(t:integer;var flag:boolean);
var i : integer;
begin
for i:=0 to 4*t do begin
sleep(2);
application.ProcessMessages;
if flag = false then break;
end;
end;

function ClearString(Str: string): string;
var
  i: Integer;
begin
  i:=0;
  while i<=Length(Str) do
    if (Str[i]=' ')or (Str[i]='''') then Delete(Str, i, 1)
    else Inc(i);
  Result:=Str;
end;

function CopySelectedText:string;
begin
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(30);
  keybd_event(Ord('C'), 0, 0, 0);
  sleep(30);
  keybd_event(Ord('C'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(30);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(500);
  result:= clipboard.AsText;
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

procedure esc;
var i:integer;
begin
    keybd_event(VK_escape, 0, 0, 0);
    sleep(50);
    keybd_event(VK_escape, 0, KEYEVENTF_KEYUP, 0);
    sleep(50);
end;

procedure pageDown(c:integer);
var i:integer;
begin
for i:=1 to c do begin
    keybd_event(VK_NEXT, 0, 0, 0);
    sleep(50);
    keybd_event(VK_NEXT, 0, KEYEVENTF_KEYUP, 0);
    sleep(50);
    end;
end;

procedure pressBackspace(c:integer);
var i:integer;
begin
for i:=1 to c do begin
    keybd_event(VK_BACK, 0, 0, 0);
    sleep(50);
    keybd_event(VK_BACK, 0, KEYEVENTF_KEYUP, 0);
    sleep(50);
    end;
end;

function randomWord(c:integer):string;
const
  Chars = 'abcdefghijklmnopqrstuvwxyz';
var
  S: string;
  i, N: integer;
begin
  S := '';
  for i := 1 to c do begin
    N := Random(Length(Chars)) + 1;
    S := S + Chars[N];
  end;
result := s;
end;

procedure reconnect;
var i:integer;
source: string;
begin
if findwindow(nil,'C:\WINDOWS\system32\cmd.exe')<>0 then begin
   destroywindow(findwindow(nil,'C:\WINDOWS\system32\cmd.exe'));
   sleep(3000);
end;
// Convert filename
source :=extractfilepath(application.ExeName)+'reconnect.bat';
ShellExecute(0,'open', pchar(source),nil, nil, SW_SHOWMINIMIZED) ;
sleep(100);
i:=0;
while findwindow(nil,'C:\WINDOWS\system32\cmd.exe')<> 0 do begin
sleep(100);
inc(i);
if i>250 then begin
   destroywindow(findwindow(nil,'C:\WINDOWS\system32\cmd.exe'));
   sleep(3000);
   break;
   end;
end;
sleep(1500);
//while findwindow('ConsoleWindowClass',nil) do sleep(100);
end;

procedure runBatAndWait(f:string);
var i:integer;
pw: pwidechar;
source: string;
filename: pWideChar;
begin
//showmessage(pchar(getsystemdir+'\cmd.exe'));
if findwindow(nil,pchar(getsystemdir+'\cmd.exe'))<>0 then begin
   destroywindow(findwindow(nil,pchar(getsystemdir+'\cmd.exe')));
   sleep(2000);
end;
// Convert filename
source :=f;
ShellExecute(0,'open', pchar(source),nil, nil, SW_HIDE) ;
sleep(100);
i:=0;
while findwindow(nil,pchar(getsystemdir+'\cmd.exe'))<> 0 do begin
sleep(100);
inc(i);
if i>250 then begin
   destroywindow(findwindow(nil,pchar(getsystemdir+'\cmd.exe')));
   sleep(3000);
   break;
   end;
end;
sleep(1500);
//while findwindow('ConsoleWindowClass',nil) do sleep(100);
end;

procedure runBat(f:string);
var i:integer;
pw: pwidechar;
source: string;
filename: pWideChar;
begin
source :=f;
ShellExecute(0,'open', pchar(source),nil, nil, SW_SHOWMINIMIZED) ;
end;

Procedure EmptyKeyQueue;
Var
  Msg: TMsg;
Begin
  While PeekMessage( Msg, 0, WM_KEYFIRST, WM_KEYLAST,
                     PM_REMOVE or PM_NOYIELD )
  Do;
End;

procedure clickat(x,y: integer);
begin
SetCursorPos(x, y); //klik na
mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
sleep(20);
mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
end;

procedure wheelUp(x,y:integer;count:integer);
var
    Pt : TPoint;
    i:integer;
begin
  SetCursorPos(x+random(4), y+random(4));
  for i:=0 to count-1 do begin
    mouse_event(MOUSEEVENTF_WHEEL , 0, 0, 10, 0);
    sleep(50);
  end;
end;

procedure clickatRnd(x,y:integer;range: integer=5);overload;
 var
    Pt : TPoint;
begin
SetCursorPos(x+random(range), y+random(range));
mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
sleep(50);
mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
//sout('clickatrnd');
end;

procedure clickatRnd(xy: tpoint;rndRange:integer=5);overload;
 var
    Pt : TPoint;
begin
SetCursorPos(xy.x+random(rndRange), xy.y+random(rndRange));
mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
sleep(50);
mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
//sout('clickatrnd');
end;

function clickAtHand(x,y: integer):integer;
begin
//curs:=screen.Cursor;
SetCursorPos(x, y); //klik na
sleep(100);
//if curs=-21 then begin
//	-21
        mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
        sleep(20);
        mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
//        end
//        else result:=false;
result:=screen.Cursor;
end;

function typeWord(str: string): string;  // wysyla keyboard eventy randomowo max dlugosc w parametrze
var i,p: integer;
begin
EmptyKeyQueue;
//keybd_event(Vk_DELETE, 0, 0, 0);
for i:=1 to length(str) do begin
       p:=pos(str[i],'~!@#$%^&*()_+}{POIUYTREWQASDFGHJKL:"?><MNBVCXZ');
      if p>0 then   keybd_event(VK_SHIFT, 0, 0, 0);

      keybd_event(VkKeyScan(str[i]), VkKeyScan(str[i]) and 128, 0, 0);
      sleep(40);
      keybd_event(VkKeyScan(str[i]), 0, KEYEVENTF_KEYUP, 0);   //enter
      sleep(40);

      if p>0 then  keybd_event(VK_SHIFT, 0, KEYEVENTF_KEYUP, 0);
      sleep(10);
    end;
result:=str;
end;

procedure pasteWord(s:string);
begin
  Clipboard.AsText:=s;
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(10);
    keybd_event(Ord('A'), 0, 0, 0);
  sleep(10);
  keybd_event(Ord('A'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(10);
  keybd_event(Ord('V'), 0, 0, 0);
  sleep(10);
  keybd_event(Ord('V'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(10);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(10);
end;

procedure pasteLink(s:string);
begin
  Clipboard.AsText:=s;
  clickat(383,66);
  sleep(100);
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(10);
    keybd_event(Ord('A'), 0, 0, 0);
  sleep(10);
  keybd_event(Ord('A'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(10);
  keybd_event(Ord('V'), 0, 0, 0);
  sleep(10);
  keybd_event(Ord('V'), 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(10);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);   //enter
  sleep(80);
  keybd_event(VK_RETURN, 0, 0, 0);
  sleep(100);
  keybd_event(VK_RETURN, 0, KEYEVENTF_KEYUP, 0);
  sleep(50);
end;

procedure scrollDown(c:integer);
var i:integer;
begin
for i:=c downto 0 do begin
    keybd_event(VK_DOWN, 0, 0, 0);
    sleep(100);
    keybd_event(VK_DOWN, 0, KEYEVENTF_KEYUP, 0);
    sleep(100);
    end;
end;


procedure ctrlTab;
begin
  EmptyKeyQueue;
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(10);
  keybd_event(VK_TAB, 0, 0, 0);
  sleep(10);
  keybd_event(VK_TAB, 0, KEYEVENTF_KEYUP, 0);
  sleep(10);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
  sleep(10);
end;

procedure ctrlA;
begin
  EmptyKeyQueue;
  keybd_event(VK_CONTROL, 0, 0, 0);
  sleep(50);
  keybd_event(ord('A'), 0, 0, 0);
  sleep(50);
  keybd_event(ord('A'), 0, KEYEVENTF_KEYUP, 0);
  sleep(50);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
  sleep(50);
end;

procedure pressEnd;
begin
  EmptyKeyQueue;
  keybd_event(VK_END, 0, 0, 0);
  sleep(50);
  keybd_event(VK_END, 0, KEYEVENTF_KEYUP, 0);
  sleep(50);
end;

procedure pressTab;
begin
  EmptyKeyQueue;
  keybd_event(VK_Tab, 0, 0, 0);
  sleep(100);
  keybd_event(VK_Tab, 0, KEYEVENTF_KEYUP, 0);
  sleep(100);
end;

procedure keyDown;
begin
  EmptyKeyQueue;
  keybd_event(VK_Down, 0, 0, 0);
  sleep(100);
  keybd_event(VK_down, 0, KEYEVENTF_KEYUP, 0);
  sleep(100);
end;

procedure pressEnter;
begin
  EmptyKeyQueue;
  keybd_event(VK_RETURN, 0, 0, 0);
  sleep(100);
  keybd_event(VK_RETURN, 0, KEYEVENTF_KEYUP, 0);
  sleep(100);
end;

procedure pressDel;
begin
  EmptyKeyQueue;
  keybd_event(VK_Delete, 0, 0, 0);
  sleep(100);
  keybd_event(VK_Delete, 0, KEYEVENTF_KEYUP, 0);
  sleep(100);
end;

procedure closeFF;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow('MozillaUIWindowClass', nil) ;
  PostMessage(AppHandle, WM_QUIT, 0, 0) ;
end;

function closeWindowByHwnd(h:Hwnd): boolean;
begin
 // AppHandle:=FindWindow('MozillaUIWindowClass', nil) ;
  result:=PostMessage(h, WM_CLOSE, 0, 0) ;
end;

function minimizeWindowByName(name: string): boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow(nil, pchar(name)) ;
  if AppHandle>0 then
  result:=PostMessage(appHandle, SW_MINIMIZE, 0, 0)
  else result:=false;
end;

procedure muteSound;
var
  i:Integer;
begin
  for i:=0 to 100 do
  begin
    keybd_event($AE, MapVirtualKey($AE,0), 0, 0);
    keybd_event($AE, MapVirtualKey($AE,0), KEYEVENTF_KEYUP, 0);
  end;
end;

procedure unMuteSound;
var
  i:Integer;
begin
  for i:=0 to 100 do
  begin
    keybd_event($AF, MapVirtualKey($AF,0), 0, 0);
    keybd_event($AF, MapVirtualKey($AF,0), KEYEVENTF_KEYUP, 0);
  end;
end;

function activateWindowByName(name: string): boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow(nil, pchar(name)) ;
  if AppHandle>0 then
  result:=PostMessage(appHandle, WM_ACTIVATE, 0, 0)
  else result:=false;
end;

function closeWindowByName(name: string): boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow(nil, pchar(name)) ;
  if AppHandle>0 then
  result:=PostMessage(appHandle, WM_CLOSE, 0, 0)
  else result:=false;
end;

function closeWindowByClass(name: string): boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow( pchar(name),nil) ;
  result:=PostMessage(appHandle, WM_CLOSE, 0, 0) ;
end;

initialization
DelayT:=ttimer.Create(nil);
DelayT.OnTimer:=evhandler.ontime;
auto_go_onWait:=true;
appPath:=extractfilepath(application.ExeName);
finalization
delayt.Free;
end.
