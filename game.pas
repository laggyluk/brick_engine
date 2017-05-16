unit game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Interfaces,  uCmdBox, dglOpenGL, windows,
  fgl, core_main, vectortypes, vectorgeometry, core_types, core_camera,
  types, core_render, core_editor, core_material_library,
  menu_pointLight,  ui_main_menu;

const
    //height of system window bar
  systemBarHeight = 38;

type

  { TUI }

  TUI = class(TForm)
    cmd: TCmdBox;
    Panel1: TPanel;
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
    procedure SetupGL ;
    procedure ErrorHandler;
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure ErrHandler(Sender: TObject; E : Exception);
  private
    { private declarations }
    StartTime, TimeCount, FrameCount  : Cardinal; //FrameCounter
    Frames, DrawTime                  : Cardinal; //& Timebased Movement
  public
    procedure init;
    { public declarations }

  end;

  procedure sout(const s:string); //outputs string to log (memo)

var
  UI: TUI;
  DC:HWND;  //rendering window handle
  RC:HWND; //rendering context
  positionBufferObject:GLuint;
  MatrixID:GLuint;
  MMatrix:GLUint;
  oldMousePos:tpoint;
  fpsTimer:dword ;
  PrevWndProc: WNDPROC;

implementation

{$R *.lfm}

{ TUI }

//for mouswheel event over panel (canvas). windows
var wtfCounter:cardinal;//every mouse wheel generates 3 same messages so need to skip
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

procedure TUI.FormCreate(Sender: TObject);
begin
  log:=@sout;
  DecimalSeparator:='.';
  Application.AddOnIdleHandler(@OnAppIdle);
  Application.OnException := @ErrHandler;
 // DC:= GetDC(panel1.handle);
  DC:= GetDC(panel1.handle);
  if not InitOpenGL then Application.Terminate;
  RC:=CreateRenderingContext(DC,[opDoubleBuffered],32, 32, 8,0,0, 0);
  //RC:=CreateRenderingContextVersion(DC,[opDoubleBuffered],4,0,true,32, 24, 0,0,0, 0);

  ActivateRenderingContext(DC, RC);
  SetupGL;
  init;
  core:=TPlanetCore.Create;
  core.init(panel1.width,panel1.height);
  //przy okazji editor being not nil is sort of operation mode flag for the engine
  editor:=teditor.create;
  fpsTimer := getTickCount;
  PrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback)));
end;

procedure TUI.FormDestroy(Sender: TObject);
begin
  editor.destroy;
  core.destroy;
end;

procedure TUI.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  cl:tcloseaction = TCloseAction.caFree;
begin
 DeactivateRenderingContext;
 DestroyRenderingContext(RC);
 ReleaseDC(Handle, DC);
end;

procedure TUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 editor.OnKeyBoard(key,true,shift);
 core.OnKeyboard( Key, true,shift);
end;

procedure TUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 editor.OnKeyBoard(key,false,shift);
 core.OnKeyboard( Key, false,shift);
end;

procedure TUI.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
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
  core.resizeViewport(panel1.clientwidth,panel1.clientheight);
end;

procedure TUI.SetupGL;
 begin

    //glDepthFunc(GL_LEQUAL);
  //glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

end ;

procedure TUI.init;
begin
  sout('OpenGL version: '+glGetString(GL_VERSION ));
end;

procedure sout(const s: string);
begin
  //logForm.memo1.lines.add(timetostr(now)+'> '+s);
  ui.cmd.Writeln(s);
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
    core.update((GetTickCount-StartTime)/ 1000);
    StartTime:= GetTickCount;
    core.Render;
    SwapBuffers(DC);
    DrawTime:= GetTickCount - StartTime;
    Inc(TimeCount,DrawTime);
    Inc(FrameCount) ;
    if TimeCount>=  1000  then  begin
      frames:= framecount;
      TimeCount := TimeCount -  1000 ;
      FrameCount :=  0 ;
      Caption := IntToStr(frames )  +  'FPS' ;
      //ErrorHandler;
    end ;
    Done :=  false ;
end;

procedure TUI.ErrHandler(Sender: TObject; E: Exception);
begin
 Log('Exception: ' + E.message);
end;

end.

