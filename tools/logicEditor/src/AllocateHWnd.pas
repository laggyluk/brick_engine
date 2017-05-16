unit AllocateHWnd;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, Winapi.Messages, Winapi.Windows;

function ThreadAllocateHWnd(AMethod: TWndMethod; MessageOnly: Boolean): HWND;
procedure ThreadDeallocateHWnd(Wnd: HWND);

implementation

const
  GWL_METHODCODE = SizeOf(Pointer)*0;
  GWL_METHODDATA = SizeOf(Pointer)*1;
  ThreadAllocateHWndClassName = 'someHk_HWnd';

var
  ThreadAllocateHWndLock: TCriticalSection;
  ThreadAllocateHWndClassRegistered: Boolean;

function ThreadAllocateHWndProc(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Proc: TMethod;
  Msg: TMessage;
begin
  Proc.Code := Pointer(GetWindowLongPtr(Window, GWL_METHODCODE));
  Proc.Data := Pointer(GetWindowLongPtr(Window, GWL_METHODDATA));
  if Assigned(TWndMethod(Proc)) then begin
    Msg.Msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := 0;
    TWndMethod(Proc)(Msg);
    Result := Msg.Result
  end else begin
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

function ThreadAllocateHWnd(AMethod: TWndMethod; MessageOnly: Boolean): HWND;

  procedure RegisterThreadAllocateHWndClass;
  var
    WndClass: TWndClass;
  begin
    if ThreadAllocateHWndClassRegistered then begin
      exit;
    end;
    ZeroMemory(@WndClass, SizeOf(WndClass));
    WndClass.lpszClassName := ThreadAllocateHWndClassName;
    WndClass.hInstance := HInstance;
    WndClass.lpfnWndProc := @ThreadAllocateHWndProc;
    WndClass.cbWndExtra := SizeOf(TMethod);
    Winapi.Windows.RegisterClass(WndClass);
    ThreadAllocateHWndClassRegistered := True;
  end;

begin
  ThreadAllocateHWndLock.Acquire;
  Try
    RegisterThreadAllocateHWndClass;
    if MessageOnly then begin
      Result := CreateWindow(ThreadAllocateHWndClassName, '', 0, 0, 0, 0, 0, HWND_MESSAGE, 0, HInstance, nil);
    end else begin
      Result := CreateWindowEx(WS_EX_TOOLWINDOW, ThreadAllocateHWndClassName, '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    end;
    Win32Check(Result<>0);
    SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(AMethod).Data));
    SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(AMethod).Code));
  Finally
    ThreadAllocateHWndLock.Release;
  End;
end;

procedure ThreadDeallocateHWnd(Wnd: HWND);
begin
  Win32Check(DestroyWindow(Wnd));
end;

initialization
  ThreadAllocateHWndLock := TCriticalSection.Create;

finalization
  ThreadAllocateHWndLock.Free;

end.