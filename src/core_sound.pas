unit core_sound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_types, core_orders,sunvox, core_orders_manager
  ;

type

  { TSoundThread }

  TSoundThread = class (TThread)
   protected
    procedure execute;override;
  end;

  { TSoundManager }

  TSoundManager = class
   public
    skipUpdate:boolean;
    soundThread:TSoundThread;
    module_weapons:integer;
    module_screams:integer;
    procedure update;
    procedure init;
    destructor destroy;override;
  end;

var
  soundManager:TSoundManager;

implementation

{ TSoundThread }

procedure TSoundThread.execute;
var
  ver,major,minor1,minor2:integer;
  n:sunvox_note;
begin
  self.FreeOnTerminate:=true;
  if ( sv_load_dll>0 ) then begin
     log('sunvox.dll not loaded!');
     exit;
  end;
  ver := sv_init( nil, 44100, 2, 0 );
  if( ver >= 0 ) then begin
    major := ( ver >> 16 ) and 255;
    minor1 := ( ver >> 8 ) and 255;
    minor2 := ( ver ) and 255;
    log( format('SunVox lib version: %d.%d.%d', [major, minor1, minor2 ]));
    //sounds slot
    sv_open_slot(0);
    sv_load(0, 'sounds'+DirectorySeparator+'sounds.sunvox');
    sv_volume(0, 256 );
    //get module numbers
    for ver:=1 to sv_get_number_of_modules(0) do begin
      case string(sv_get_module_name(0,ver-1)) of
       'weapons':soundManager.module_weapons:=ver;
       'screams':soundManager.module_screams:=ver;
      end;
    end;
    //music slot
    sv_open_slot( 1 );
    sv_load( 1, 'test.sunvox' );
    sv_volume( 1, 256 );
    //sv_play( 1 );
    //log( format('Line counter: %d\n', [sv_get_current_line( 0 )] ));
    //log( 'Music! SunVox DLL');
    //log( format('Line counter: %d\n', [sv_get_current_line( 0 )] ));
    //listen for events
    while not self.Terminated do begin
      sleep(500);
     // sv_send_event(0,0,80,0,4,0,0);
    end;
  end
  else begin
     log( format('sv_init() error %d\n', [ver]) );
  end;
  sv_stop( 0 );
  sv_close_slot( 0 );
  sv_deinit();
  sv_unload_dll();
end;

{ TSoundManager }

procedure TSoundManager.update;
var
  i:integer;
  cmd:rOrder;
  wtf:integer;
begin
 wtf:=oM.queue.Count;
 for i:=0 to oM.queue.Count-1 do begin
    cmd:=oM.queue[i];
    if cmd.eatenFlags.eatenSound then continue;
    with cmd do  begin
      eatenFlags.eatenSound:=true;
      case order of
       //play birth sound? ;p
       orCreateActor: sv_send_event(0,0,50,0,module_screams,0,0);
       orBlasterShot: sv_send_event(0,0,13,0,module_weapons,0,0);
      end;
    end;
    oM.queue[i]:=cmd;
 end;
end;

procedure TSoundManager.init;
var
  ver,major,minor1,minor2:integer;
begin
  soundThread:=tsoundThread.Create(false);

end;

destructor TSoundManager.destroy;
begin
  soundThread.terminate;
  soundThread.WaitFor;
 {
  if libLoaded then begin
     sv_stop( 0 );
     sv_close_slot( 0 );
     sv_deinit();
     sv_unload_dll();
  end;
  }
  inherited destroy;
end;

end.

