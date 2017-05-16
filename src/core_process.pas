unit core_process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  processTypes = (
    ptWait //waits for given time. ment for delayed launch of child process
    );

  { TProcess }

  TProcess = class
    kill:boolean;
    active:boolean;
    paused:boolean;
    initialUpdate:boolean;
    procType:processTypes;
    next:TProcess;
    flags:integer;
    constructor create(typ:processTypes;order:integer=0);
    //overloadables
    procedure vInitialize;virtual;
    procedure vUpdate;virtual;
    procedure vKill;virtual;
    procedure vTogglePause;virtual;
  end;
  TProcessList = specialize TFPGList<TProcess>;

  { TProcessManager }

  TProcessManager  = class
    processList:TProcessList;
    procedure attach(proc:TProcess);
    procedure update;
    constructor create;
    destructor destroy;
   private
    procedure Detach(proc:TProcess);
  end;

var
  processManager:TProcessManager;

implementation

constructor TProcess.create(typ: processTypes; order: integer);
begin
  procType:=typ;
  initialUpdate:=true;
end;

procedure TProcess.vInitialize;
begin
{place any initialization code. It‘s a better practice to place initialization
code here since you can actually mark the process dead before it even runs.}
end;

procedure TProcess.vUpdate;
begin
  if initialUpdate then begin
    vInitialize;
    initialUpdate:=false;
  end;
end;

procedure TProcess.vKill;
begin

end;

procedure TProcess.vTogglePause;
begin

end;

{ TProcessManager }

procedure TProcessManager.attach(proc: TProcess);
begin

end;

procedure TProcessManager.update;
var
   p:tprocess;
   next:tprocess;
begin
  for p in processList do begin
    if p.kill then begin
       next:=p.next;
       if next<>nil then begin
          p.next:=nil;
          attach(next);
       end;
       detach(p);//if process aint marked for deletion then update
    end else if (p.active) and (not p.paused) then p.vupdate;
  end;
end;

constructor TProcessManager.create;
begin
  processList:=TProcessList.create;
end;

destructor TProcessManager.destroy;
var
  p:tprocess;
begin
  for p in processList do p.Destroy;
  processList.Destroy;
end;

procedure TProcessManager.Detach(proc: TProcess);
begin
  //what about actor that spawned this proc?
  proc.Free;
  proc:=nil;
  processList.Remove(proc);
end;

end.

