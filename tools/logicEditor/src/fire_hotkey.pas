unit fire_hotkey;

interface

uses windows, messages,allocatehwnd;

type
  TMsgHandler = procedure (var Msg: TMessage) of object;

  THotClass = class(TObject)
    fMsgHandlerHWND : HWND;
    proc:TMsgHandler;
    constructor Create;
    procedure init;
    destructor Destroy; override;
  end;

implementation

{ hotClass }

constructor THotClass.Create;
begin
  inherited;

end;

destructor THotClass.Destroy;
begin
  ThreadDeallocateHWnd(fMsgHandlerHWND);
  inherited;
end;

procedure THotClass.init;
begin
   fMsgHandlerHWND := ThreadAllocateHWnd(proc,true);
end;

end.
