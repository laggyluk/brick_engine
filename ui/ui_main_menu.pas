unit ui_main_menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_ui_controls, core_ui_base, core_texture, core_types;

type

  TStringParamProc = procedure (s:string) of object;

  { TMainMenu }

  TMainMenu = class (TUICanvas)
    newGameBtn:TUIButton;
    //we have no access to some systems here, let them assign own functions
    LoadLevelCallback:TStringParamProc;
    procedure NewGameBtnClick(sender:TObject;Button: integer; down:boolean;Shift: TShiftState; mX,mY: Integer);
    constructor Create(_atlas: TTextureAtlas);
  end;

implementation

{ TMainMenu }

procedure TMainMenu.NewGameBtnClick(sender:tobject;button: integer;down:boolean;Shift: TShiftState; mX, mY: Integer);
begin
  //single player mode

  //lock mouse
//  inputManager.mouseLocked:=true;
  //show crosshairs
  //uiManager.toggleCrosshair(true);
  if down then begin
    log('new game');
    LoadLevelCallback(format('%s%s%s%s',[appPath,'saves',DirectorySeparator,'map.terrain']));
    visible:=false;
    //game.loadGame(format('%s%s%s%s',[appPath,'saves',DirectorySeparator,'map.terrain']),false);
  end;
end;

constructor TMainMenu.Create(_atlas:TTextureAtlas);
begin
  inherited create(nil);
  width:=1;
  height:=1;
  newGameBtn:=TUIButton.create(self,_atlas,-0.15,0,0.3,0.07,32);
  newGameBtn.caption.text:='new game';
  newGameBtn.OnMouseClick:=@NewGameBtnClick;
end;

end.

