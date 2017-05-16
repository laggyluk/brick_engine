unit core_ui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, core_types, core_ui_controls,
  controls, core_texture, core_material_library, fgl, core_listofrecords,
  VectorGeometry, core_ui_base, ui_main_menu;

type

  { TuiManager }
  TuiManager = class
   private
    atlas:TTextureAtlas;
    canvasy:TList;
    topMenu:TUICanvas;
    uiLogg:TUILog;
   public
    mainMenu:TMainMenu;
    procedure init;
    //closes active menu window and changes currentWindow to next active window
    //so esc key can successively close all open windows
    procedure resize(w,h:integer);
    //returns true if one of forms handled key
    function OnKeyBoard( var Key: Word; down: boolean; Shift: TShiftState):boolean;
    //returns true if click was on ui element
    function OnMouseClick(Button: integer; down:boolean;Shift: TShiftState; mX,mY: Integer):boolean;
    procedure ClickTest(sender:tobject;button: integer;down:boolean;Shift: TShiftState; X, Y: Integer);
    //draw crosshairs and shit
    procedure render;
    procedure Update;
    constructor Create;
    destructor Destroy;override;
  end;

  procedure glLogMsg(const s:string);

var
  //global font manager
  uiManager:TuiManager;

implementation


procedure glLogMsg(const s: string);
begin
  //uiManager.uiLog.add(s);
  if uiManager.uiLogg<>nil then uiManager.uiLogg.Add(s);
end;


procedure TuiManager.init;
var
  i:  integer;
  testRect,testBtn,testBtn1:TUIRect;
  txt:TUIText;
begin
  //initialize bitmap fonts
  matlib.shaderUI.use;
  atlas:=TTextureAtlas.create(GL_TEXTURE_2D);
  atlas.load(apppath+'textures\ui.atlas',false);
  i:=atlas.getIndexOf('crossHair0');
  //i:=atlas.getIndexOf('E');

  //Create log
  uiLogg:=TUILog.create(atlas);
  canvasy.Add(uiLogg);

  mainMenu:=TMainMenu.create(atlas);
  canvasy.Add(mainMenu);
  mainMenu.build;
  mainMenu.visible:=false;
{
  topMenu:=TUICanvas.Create(nil);
  canvasy.add(topMenu);

  testRect:=TUIRect.Create(topmenu);
  testRect.sprite:=@atlas.data[atlas.getIndexOf('rect')];
  testRect.width:=1;
  testRect.height:=0.1;
  testRect.x:=-0.5;
  testRect.y:=0.4;
  testRect.color[0]:=0.1;
  testRect.color[1]:=0.1;
  testRect.color[2]:=0.1;
  testRect.color[3]:=0.5;
  testRect.name:='testRect';

  testBtn:=TUIRect.Create(testRect);
  testBtn.sprite:=@atlas.data[atlas.getIndexOf('roundRect')];
  testBtn.width:=0.1;
  testBtn.height:=0.9;
  testBtn.x:=0.6;
  testBtn.y:=0.05;
  testBtn.color[0]:=1;
  testBtn.color[1]:=1;
  testBtn.color[2]:=1;
  testBtn.color[3]:=1;
  testBtn.visible:=true;
  testBtn.hitTest:=true;
  testBtn.enabled:=false;
  testBtn.NewGameBtnClick:=@ClickTest;
  testBtn.name:='testBtn';

  testBtn1:=TUIRect.Create(testRect);
  testBtn1.sprite:=@atlas.data[atlas.getIndexOf('roundRect')];
  testBtn1.width:=0.1;
  testBtn1.height:=0.9;
  testbtn1.x:=0.2;
  testBtn1.hitTest:=true;
  testBtn1.NewGameBtnClick:=@ClickTest;
  testBtn1.name:='testBtn1';


  txt:=tuiText.Create(testBtn1,atlas);
  txt.width:=0.8;
  txt.height:=1;
  txt.x:=0.1;
  txt.y:=0.2;
  txt.text:='hello';

  topMenu.Build;
}
end;

procedure TuiManager.render;
var i:integer;
begin
  matlib.shaderUI.use;
  atlas.Bind(GL_TEXTURE0);//???
  //glUniform2f(matlib.shaderUI.texSize,round(Atlas.texture.img.png.Width),round(Atlas.texture.img.png.height));
  //set uniforms
  //texQuad.mvpLocation:=matlib.shaderUI.MVP;
  //texQuad.worldMatricLocation:=matlib.shaderUI.m_WorldMatrixLocation;
  for i:=0 to canvasy.Count-1 do with TUICanvas(canvasy[i]) do if visible then render;
end;

procedure TuiManager.resize(w, h: integer);
begin
 log('achtung! TuiManager.resize:: not implemented');
end;

function TuiManager.OnKeyBoard(var Key: Word; down: boolean; Shift: TShiftState
  ): boolean;
begin
  result:=true;
  //main game window
{  if currentForm=ui then
  case key of
    vk_f2:ui.itemsBtnMouseUp(self,mbLeft,[],0,0);
    vk_f1:ui.buildBtnMouseUp(self,mbLeft,[],0,0);
    else result:=false;
  end
  //build menu
  else if currentForm=menuItems then
  case key of
    VK_ESCAPE:closeForm;
    else result:=false;
  end
  //main menu form
  else if currentForm=menuForm then
  case key of
    VK_ESCAPE:closeForm;
    else result:=false;
  end;
  }
end;

function TuiManager.OnMouseClick(Button: integer; down: boolean;
  Shift: TShiftState; mX, mY: Integer): boolean;
var
  i:integer;
  e:TUITransform;
begin
  result:=false;
  for i:=0 to canvasy.count-1 do with TUICanvas(canvasy[i]) do begin
     if (not visible) or (not enabled) then continue;
     e:=checkKidsHitTest(mx,my);
     if (e<>nil) and (assigned(e.OnMouseClick)) then begin
       //there's no z-check so if ever i'll have overlaping buttons only first found will be clicked
       //maybe could be prevented by swapping canvas order in this list whenever one of them is clicked?
       //still is ok to have only one canvas .enabled at a time
       e.OnMouseClick(e,button,down,shift,mx,my);
       result:=true;
       break;
     end;
  end;
end;

procedure TuiManager.ClickTest(sender:tobject;button: integer; down: boolean;
  Shift: TShiftState; X, Y: Integer);
begin
  log('click!');
  //uiLogg.Add('asd f asdfasdf');
end;

procedure TuiManager.Update;
var
  i:integer;
begin
  for i:=0 to canvasy.Count-1 do TUICanvas(canvasy[i]).Update;
end;

constructor TuiManager.Create;
begin
  canvasy:=tlist.create;
end;

destructor TuiManager.Destroy;
var i:integer;
begin
  for i:=0 to canvasy.Count-1 do TUICanvas(canvasy[i]).free;
  canvasy.free;
  atlas.Free;
end;

end.

