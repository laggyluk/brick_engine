unit brickUImain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Interfaces, dglOpenGL, core_main,
  vectorgeometry, core_types, types, core_render,
  core_editor,
  core_ui, core_game,
  toolbar_log,core_editor_states,
  core_input //to lock mouse
  ;

const
    //height of system window bar
  systemBarHeight = 38;
  cursorDig = 1;

type

  { TUI }

  TUI = class(TForm)
    initTimer: TTimer;
    updateTimer: TTimer;
    procedure buildBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure digBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure initTimerTimer(Sender: TObject);
    procedure itemsBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mainMenuBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mapBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1Click(Sender: TObject);
    procedure planBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure separator4Click(Sender: TObject);
    procedure separator4Resize(Sender: TObject);
    procedure SetupGL ;
    procedure ErrorHandler;
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure ErrHandler(Sender: TObject; E : Exception);
    procedure updateTimerTimer(Sender: TObject);
  private
    { private declarations }
    StartTime, TimeCount, FrameCount  : Cardinal; //FrameCounter
    Frames, DrawTime                  : Cardinal; //& Timebased Movement
    procedure update;
  public
    //call it from toolbar form to allow conditional 'reaction' to regaining focus. what am I doing?
    procedure FocusFromToolbar(toolbarName:string);
    procedure init;
  end;

  procedure sout(const s:string); //outputs string to log (memo)

var
  UI: TUI;
  DC:cardinal;  //rendering window handle
  RC:cardinal; //rendering context
  positionBufferObject:GLuint;
  MatrixID:GLuint;
  MMatrix:GLUint;
  oldMousePos:tpoint;
  fpsTimer:dword ;
  {$ifdef windows}
    PrevWndProc: WNDPROC;
  {$endif}


implementation

{$R *.lfm}

{ TUI }

//for mouswheel event over panel (canvas). windows
var wtfCounter:cardinal;//every mouse wheel generates 3 same messages so need to skip
{$ifdef windows}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
   pt: TPoint;
   i: Integer;
   delta: ShortInt;
   wheel_delta: Word;
   rotation: Double;
   handled:boolean;
   sh:tshiftstate;
begin
  if (uMsg=WM_MOUSEWHEEL) and (wtfCounter mod 3=0)then
  begin
    GetCursorPos(pt);
    wheel_delta := HiWord(WParam);
    Move(wheel_delta, delta, SizeOf(Word));
    if delta>0 then delta:=1;
    if delta<0 then delta:=-1;
    //no shift state
    if GetKeyState(VK_SHIFT) < 0 then sh+=[ssShift];
    ui.FormMouseWheel(nil,sh,delta,pt,handled);
  end;
  inc(wtfCounter);
  result:=CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;
{$endif}

procedure TUI.FormCreate(Sender: TObject);
begin
  log:=@sout;
  log_add:=@sout;
  DecimalSeparator:='.';

 // Application.AddOnIdleHandler(@OnAppIdle);
  Application.OnException := @ErrHandler;
 // DC:= GetDC(panel1.handle);
  DC:= LCLIntf.GetDC(handle);
  if not InitOpenGL then Application.Terminate;
  {$ifdef windows}
    RC:=CreateRenderingContext(DC,[opDoubleBuffered],32, 32, 8,0,0, 0);
    //RC:=CreateRenderingContextVersion(DC,[opDoubleBuffered],4,0,true,32, 24, 8,0,0, 0);
    ActivateRenderingContext(DC, RC);
    PrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback)));
  {$endif}
  SetupGL;
  init;
  core:=TPlanetCore.Create;
  core.init(width,height);
  //przy okazji editor being not nil is sort of operation mode flag for the engine
  editor:=teditor.create;
  fpsTimer := getTickCount;

  initTimer.Enabled:=true;
end;

procedure TUI.FormDestroy(Sender: TObject);
begin

end;

procedure TUI.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  cl:tcloseaction = TCloseAction.caFree;
begin
  updateTimer.enabled:=false;
  editor.free;
  core.free;
// if gameMode then game.saveGame(appPath+'saves'+DirectorySeparator);
 {$ifdef windows}
   DeactivateRenderingContext;
   DestroyRenderingContext(RC);
 {$endif}
 ReleaseDC(Handle, DC);
 application.Terminate;
end;

procedure TUI.buildBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 {
  menuBuild.populateList;
  menuBuild.Show;
  panel1.Cursor:=0;
  uiManager.currentForm:=menuBuild;
  }
end;

procedure TUI.Button1Click(Sender: TObject);
begin
    //single player mode
  game.loadGame(format('%s%s%s%s',[appPath,'saves',DirectorySeparator,'map.terrain']));
  //lock mouse
  inputManager.mouseLocked:=true;
end;

procedure TUI.digBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  core.inputState:=iFreeZone;
  Cursor:=cursorDig;
  renderer.gizmos.brush:=false;
end;

procedure TUI.FormActivate(Sender: TObject);
begin

end;

procedure TUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 editor.OnKeyBoard(key,true,shift);
 core.OnKeyboard( Key, true,shift);
 case key of
   vk_f2:ui.itemsBtnMouseUp(self,mbLeft,[],0,0);
   vk_f1:ui.buildBtnMouseUp(self,mbLeft,[],0,0);
 end;
 if (key = 27) then begin
     //uiManager.CloseCurrentForm;
     inputManager.mouseLocked:=not inputManager.mouseLocked;
 end;
 //show 'console'
 if key = VK_OEM_3 then logForm.WindowState:=wsNormal;

 if (key = VK_Z) and (ssShift in shift) and (ssCtrl in shift)  then editor.redo else
   if (key = VK_Z) and (ssCtrl in shift) then editor.undo;
end;

procedure TUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 editor.OnKeyBoard(key,false,shift);
 core.OnKeyboard( Key, false,shift);
end;

procedure TUI.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   //first check if it's an ui click
  if uiManager.OnMouseClick(integer(button),true,shift,x,y) then exit;
  if editor<>nil then editor.OnMouseClick(integer(button),true,shift,x,y);
  core.OnMouseClick(integer(Button),true,Shift,X,Y);
end;

procedure TUI.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ui.Active then core.OnMouseMove(Shift, X, Y);
  oldMousePos.x:=x;
  oldMousePos.y:=y;
end;

procedure TUI.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   //first check if it's an ui click
  if uiManager.OnMouseClick(integer(button),false,shift,x,y) then exit;
  if editor<>nil then editor.OnMouseClick(integer(button),false,shift,x,y);
  core.OnMouseClick(integer(Button),false,Shift, X,y);
end;

procedure TUI.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  core.OnMouseWheel(shift,wheelDelta,mousePos);
  editor.OnMouseWheel(shift,wheelDelta,mousePos);
end;

procedure TUI.FormResize(Sender: TObject);
var tmpBool : Boolean;
begin
  core.resizeViewport(clientwidth,clientheight);
  inputManager.winCenterX:=left+ui.width div 2;
  inputManager.winCenterY:=Top+ (Height div 2);
  uiManager.resize(clientwidth,clientheight);
end;

procedure TUI.initTimerTimer(Sender: TObject);
var cur:TCursorImage;
begin
  initTimer.Enabled:=false;
  //because I'm using stupid ttf font loaded at runtime bitmap are labels/buttons
  //and need text being painted on them on init/resize
{  uiManager.paintFormButtons(self);
  uiManager.paintFormButtons(menuBuild);
  uiManager.paintFormButtons(menuItems);}
  cur:=TCursorImage.create;
  cur.LoadFromFile(apppath+DirectorySeparator+'textures'+DirectorySeparator+'cursorDig.ico');
  //Screen.Cursors[1] :=LoadCursorFromLazarusResource('cursorDig');
  Screen.Cursors[1] :=cur.ReleaseHandle;
  cur.free;
  BringToFront;
//  if gameMode then menuForm.show;
end;

procedure TUI.itemsBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //uiManager.centerForm(menuItems);
  //uiManager.currentForm:=menuItems;
  //menuItems.populateList;
  //menuItems.Show;
  //panel1.Cursor:=0;
  //core.inputState:=iSelect;
end;

procedure TUI.mainMenuBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TUI.mapBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TUI.Panel1Click(Sender: TObject);
begin
  inputManager.mouseLocked:=true;
end;

procedure TUI.planBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TUI.separator4Click(Sender: TObject);
begin

end;

procedure TUI.separator4Resize(Sender: TObject);
begin

end;

procedure TUI.SetupGL;
begin
  //glDepthFunc(GL_LEQUAL);
  //glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
  glEnable(GL_SCISSOR_TEST);
end ;

procedure TUI.init;
begin
  sout('OpenGL version: '+glGetString(GL_VERSION ));
  sout('vendor: '+glGetString(GL_VENDOR ));
  sout('renderer: '+glGetString(GL_RENDERER ));
end;

procedure sout(const s: string);
begin
  logForm.memo1.lines.add(timetostr(now)+'> '+s);
  //logForm.cmd.Writeln(s);
end;

procedure TUI.ErrorHandler;
var s:string;
begin
    s:=gluErrorString(glGetError());
    if s<>'no error' then UI.Caption :=s;
end;

procedure TUI.OnAppIdle(Sender: TObject; var Done: Boolean);
var
   t:integer;
begin
   update;
   Done :=  false ;
end;

procedure TUI.ErrHandler(Sender: TObject; E: Exception);
begin
 Log('Exception: ' + E.message);
end;

procedure TUI.updateTimerTimer(Sender: TObject);
begin
  update;
end;

procedure TUI.FocusFromToolbar(toolbarName: string);
begin
  BringToFront();
  if toolbarName = 'toolbarFlats' then editor.state:=esActor;
end;

procedure TUI.update;
begin
 core.update((GetTickCount64-StartTime)/ 1000);
     StartTime:= GetTickCount64;
     core.Render;
     {$ifdef windows}
     SwapBuffers(DC);
     {$endif}
     {$ifdef linux}
     TglXSwapBuffers(
     {$endif}
     DrawTime:= GetTickCount64 - StartTime;
 {    Inc(TimeCount,DrawTime);
     Inc(FrameCount) ;
     if TimeCount>=  1000  then  begin
       frames:= framecount;
       TimeCount := TimeCount -  1000 ;
       FrameCount :=  0 ;
       Caption := IntToStr(frames )  +  'FPS' ;
       //ErrorHandler;
     end ;
     }
end;


end.

