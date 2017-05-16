unit topPanelUnit;

{$mode objfpc}{$H+}

{$DEFINE EDITOR}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Buttons,
  StdCtrls, ExtCtrls, Menus, ComCtrls, core_terrain_generator, core_main,
  core_types, core_render, editor_utils, core_editor, toolbar_brushes,
  toolbar_lights, blocksToolbarUnit,  toolbar_game_objs, inifiles,
  core_behaviour_tree, core_logic, lNetComponents,
  toolbar_newMap, map_properties_form, core_camera, core_chunk,
  toolbar_plant_generator,  windows, core_atmosphere, VectorGeometry,
  toolbar_flat_units, toolbar_log, core_mesh, ui_list_of_actors,
  toolbar_terrain,core_game, core_editor_states,brickUIMain;

type

  { TtopPanel }

  TtopPanel = class(TForm)
    animatedToolbarsBtn: TBitBtn;
    atmosphereEnabled: TCheckBox;
    listMapActors: TBitBtn;
    menuItemCutEdge: TMenuItem;
    menuItemReset: TMenuItem;
    menuItemLogForm: TMenuItem;
    menuItemWindows: TMenuItem;
    ssaoEnabled: TCheckBox;
    initTimer: TTimer;
    menuSaveObj: TMenuItem;
    menuItemObject: TMenuItem;
    oxygnRenderBtn: TToggleBox;
    logicEditorPath: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveMap: TMenuItem;
    MenuItemOpenMap: TMenuItem;
    MenuItemSaveAsBrush: TMenuItem;
    menuItemMapOptions: TMenuItem;
    menuSetLogicPath: TMenuItem;
    menuOptions: TMenuItem;
    invisibleBlockingBtn: TToggleBox;
    plantsToolbrBtn: TBitBtn;
    brainsBtn: TSpeedButton;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuNewMap: TMenuItem;
    remoteBrainEnabled: TSpeedButton;
    saveMapAsObj: TSaveDialog;
    SaveDialogAsBrush: TSaveDialog;
    saveActors: TBitBtn;
    SelectDirectory: TSelectDirectoryDialog;
    gameSelectionBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    oxygenEmitterBtn: TSpeedButton;
    SpeedButton2: TSpeedButton;
    waterEmitterBtn: TSpeedButton;
    udpPort: TEdit;
    ludp: TLUDPComponent;
    loadMapDlg: TOpenDialog;
    saveMapDlg: TSaveDialog;
    saveMapAsBtn: TSpeedButton;
    terrainBtn: TBitBtn;
    GroupBox1: TGroupBox;
    lightGizmos: TToggleBox;
    loadMapBtn: TBitBtn;
    selectMode: TBitBtn;
    saveMapBtn: TBitBtn;
    procedure animatedToolbarsBtnClick(Sender: TObject);
    procedure atmosphereEnabledChange(Sender: TObject);
    procedure brainsBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gameSelectionBtnClick(Sender: TObject);
    procedure initTimerTimer(Sender: TObject);
    procedure invisibleBlockingBtnClick(Sender: TObject);
    procedure listMapActorsClick(Sender: TObject);
    procedure loadMapBtnClick(Sender: TObject);
    procedure menuItemCutEdgeClick(Sender: TObject);
    procedure menuItemLogFormClick(Sender: TObject);
    procedure menuItemResetClick(Sender: TObject);
    procedure menuItemWindowsClick(Sender: TObject);
    procedure menuNewObjClick(Sender: TObject);
    procedure menuSaveObjClick(Sender: TObject);
    procedure menuItemObjectClick(Sender: TObject);
    procedure menuItemMapOptionsClick(Sender: TObject);
    procedure MenuItemOpenMapClick(Sender: TObject);
    procedure MenuItemSaveAsBrushClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveMapClick(Sender: TObject);
    procedure menuNewMapClick(Sender: TObject);
    procedure menuSetLogicPathClick(Sender: TObject);
    procedure oxygenEmitterBtnClick(Sender: TObject);
    procedure oxygnRenderBtnClick(Sender: TObject);
    procedure plantsToolbrBtnClick(Sender: TObject);
    procedure remoteBrainEnabledClick(Sender: TObject);
    procedure saveActorsClick(Sender: TObject);
    procedure saveMapAsBtnClick(Sender: TObject);
    procedure saveMapBtnClick(Sender: TObject);
    procedure selectModeClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ssaoEnabledChange(Sender: TObject);
    procedure terrainBtnClick(Sender: TObject);
    procedure waterEmitterBtnClick(Sender: TObject);
  private
    { private declarations }
    terrainToolbars:boolean;
    windows:tlist;
    wtfTextEditorPath:string;
  public
    procedure saveConfig;
    { public declarations }
  end;

  //for remote brain debugging. called from core_behaviour_tree
  procedure sendRemoteBrainMsg(const s:string);

var
  topPanel: TtopPanel;

implementation


procedure sendRemoteBrainMsg(const s: string);
var
  n:integer;
begin
  n := toppanel.ludp.SendMessage(s, '255.255.255.255');
  if n < Length(s) then
    log('Error on send [' + IntToStr(n) + ']')
   // else
      //log('message sent: ' + s);
end;

{$R *.lfm}


procedure FormPosition(f:tform;load:boolean);
var ini:tinifile;
begin
   ini:=tinifile.create(apppath+'editor.ini');
   if load then begin
      f.left:=ini.ReadInteger(f.Name,'left',100);
      f.top:=ini.ReadInteger(f.Name,'top',100);
      f.width:=ini.ReadInteger(f.Name,'width',100);
      f.height:=ini.ReadInteger(f.Name,'height',100);
   end
   else begin
        ini.writeInteger(f.Name,'left',f.left);
        ini.writeInteger(f.Name,'top',f.top);
        ini.writeInteger(f.Name,'width',f.width);
        ini.writeInteger(f.Name,'height',f.height);
   end;
   ini.free;
end;

{ TtopPanel }

procedure TtopPanel.saveMapBtnClick(Sender: TObject);
begin
  if terrain.mapfilename='' then saveMapAsBtn.Click else begin
    core.saveMap(terrain.mapFileName);
    clearUndoHistory;
  end;
end;

procedure TtopPanel.selectModeClick(Sender: TObject);
begin
  editor.state:=esSelectMode;
  ui.BringToFront;
end;

procedure TtopPanel.SpeedButton1Click(Sender: TObject);
begin
  //uiManager.screen:=(stMainMenu);
  ui.BringToFront;
end;

procedure TtopPanel.SpeedButton2Click(Sender: TObject);
begin
  ShellExecute(handle,'open',PChar('wtfTextEditor.exe'), '',PChar(wtfTextEditorPath),SW_SHOWNORMAL);
end;

procedure TtopPanel.ssaoEnabledChange(Sender: TObject);
begin
  options.renderer.ssao:=(sender as tcheckbox).Checked;
end;

procedure TtopPanel.terrainBtnClick(Sender: TObject);
begin
  terrainToolbars:= not terrainToolbars;
  //toolbarLights.Visible:=terrainToolbars;
  //toolbarBrushes.Visible:=terrainToolbars;
  toolbarLights.Visible:=true;
  toolbarBrushes.Visible:=true;
  toolbarBlocks.visible:=true;

  toolbarLights.BringToFront;
  toolbarBrushes.BringToFront;
  toolbarBlocks.BringToFront;
end;

procedure TtopPanel.waterEmitterBtnClick(Sender: TObject);
begin
  editor.state:=esWaterEmitter;
  ui.BringToFront;
end;

procedure TtopPanel.saveConfig;
var ini:tinifile;
begin
  savecontrol(lightGizmos);
  savecontrol(udpport);
  savecontrol(logicEditorPath);
  ini:=tinifile.create(apppath+'editor.ini');
//  ini.writeString('options','mapFile',terrain.mapFileName);
  ini.free;
end;

procedure TtopPanel.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TtopPanel.animatedToolbarsBtnClick(Sender: TObject);
begin
  toolbarGameObjs.visible:=true;
  toolbarGameObjs.BringToFront;
end;

procedure TtopPanel.atmosphereEnabledChange(Sender: TObject);
begin
  if atmosphereEnabled.checked then atmosphere.resume else atmosphere.pause;
end;

procedure TtopPanel.brainsBtnClick(Sender: TObject);
begin
  ShellExecute(handle,'open',PChar('LogicEditor.exe'), '',PChar(logicEditorPath.Text),SW_SHOWNORMAL);
end;

procedure TtopPanel.Button1Click(Sender: TObject);
begin

end;

procedure TtopPanel.FormCreate(Sender: TObject);
var ini:tinifile;
begin
  loadControl(lightGizmos);
  loadControl(logicEditorPath);
  loadControl(udpPort);
  ini:=tinifile.create(appPath+'editor.ini');
  wtfTextEditorPath:=ini.readstring('options','wtfTextEditorPath','');
  terrainToolbars:=true;
  remoteBrainDebugMsg:=@sendRemoteBrainMsg;
  //logic.skipUpdate:=true;
  topPanel.Left:=0;
  topPanel.Top:=0;
//  topPanel.Width:=ui.Width+toolbarBlocks.Width+10;
  windows:=tlist.create;
  windows.Add(logForm);
  windows.Add(toolbarBlocks);
  windows.Add(toolbarBrushes);
  windows.Add(toolbarTerra);
  windows.Add(toolbarFlats);
  windows.Add(toolbarLights);
  windows.Add(ui);
  windows.Add(actorsListForm);
end;

procedure TtopPanel.FormDestroy(Sender: TObject);
var i:integer;
begin
  saveConfig;
  for i:=0 to windows.count-1 do begin
      FormPosition(tform(windows[i]),false);
  end;
  windows.free;
end;

procedure TtopPanel.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  log('key down');
end;

procedure TtopPanel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

end;

procedure TtopPanel.gameSelectionBtnClick(Sender: TObject);
begin
  editor.state:=esDrawSelection;
  ui.BringToFront;
end;

procedure TtopPanel.initTimerTimer(Sender: TObject);
var i:integer;
begin
  initTimer.Enabled:=false;
  ui.show;

  for i:=0 to windows.count-1 do begin
      FormPosition(tform(windows[i]),true);
  end;

  toolbarBlocks.populateColorList;
  toolbarBlocks.colorList.ItemIndex:=0;

  if topPanel.ludp.Listen then begin
    log('AI Server started');
  end;
  toolbarTerra.show;
  logForm.Show;
  toolbarLights.show;
  toolbarFlats.show;
  toolbarBrushes.show;
  toolbarBlocks.show;
  toolbarTerra.populateList;

  ui.bringtofront;
end;

procedure TtopPanel.invisibleBlockingBtnClick(Sender: TObject);
begin
  if btInvisibleBlocking in mineralsDontRender then mineralsDontRender:=mineralsDontRender-[btInvisibleBlocking]
  else begin
       mineralsDontRender:=mineralsDontRender+[btInvisibleBlocking];
  end;
  terrain.cullAllChunks;
  ui.BringToFront;
end;

procedure TtopPanel.listMapActorsClick(Sender: TObject);
begin
  actorsListForm.show;
end;

procedure TtopPanel.loadMapBtnClick(Sender: TObject);
var s:string;
begin
  loadMapDlg.InitialDir:=apppath+DirectorySeparator+'terrain';
  if loadMapDlg.Execute then begin
     //om.addOrder2i(orMapLoad,0,0);
     core.update(1);
     terrain.loadMap(loadMapDlg.FileName);
     s:=loadMapDlg.FileName;
     s:=ChangeFileExt(s,'.actors');
     game.loadActorsFromFile(s);
     toolbarBlocks.populateColorList;
     toolbarBlocks.colorGrid.clean;
     clearUndoHistory;
//     camera.setOrigin(0,150,50);
     camera.setTarget(0,chunk_height2 div 2,0);
     camera.setOrigin(0,chunk_height,80);
     //om.addOrder2i(orMapLoaded,0,0);
  end;
end;

procedure TtopPanel.menuItemCutEdgeClick(Sender: TObject);
var
  chx,x,y:integer;
begin
  for chx:=0 to active_chunks_w-1 do begin
    for x:=0 to chunk_width-1 do for y:=0 to chunk_height-1 do begin
      chunksPointer^[chx,active_chunks_h-1].blocks[x,y,chunk_width-1]:=0;
    end;
    chunksPointer^[chx,active_chunks_h-1].reBuild:=true;
  end;
end;

procedure TtopPanel.menuItemLogFormClick(Sender: TObject);
begin
  logform.Show;
end;

procedure TtopPanel.menuItemResetClick(Sender: TObject);
var i:integer;
begin
  for I:=0 to windows.Count-1 do with tform(windows[i]) do begin
    left:=0;
    top:=0;
  end;
end;

procedure TtopPanel.menuItemWindowsClick(Sender: TObject);
begin

end;

procedure TtopPanel.menuNewObjClick(Sender: TObject);
begin
  core.setMapSize(32 ,64,1,1);
  terrain.initMap(true);
end;

procedure TtopPanel.menuSaveObjClick(Sender: TObject);
begin
  saveMapAsObj.InitialDir:=modelsFolder;
  if saveMapAsObj.Execute then begin
    terrain.saveVboBuffToFile(saveMapAsObj.FileName);
    meshManager.loadModelsFromFolder(modelsFolder);
  end;
end;

procedure TtopPanel.menuItemObjectClick(Sender: TObject);
begin

end;

procedure TtopPanel.menuItemMapOptionsClick(Sender: TObject);
begin
  mapProperties.mapName.text:=terrain.mapFileName;
  mapProperties.description.Lines.Text:=terrain.description;
  mapProperties.UpDown1.Position:=terrain.maxPlayers;
  mapProperties.showModal;
end;

procedure TtopPanel.MenuItemOpenMapClick(Sender: TObject);
begin
  loadMapBtn.Click;
end;

procedure TtopPanel.MenuItemSaveAsBrushClick(Sender: TObject);
var
  filename:string;
begin
  SaveDialogAsBrush.InitialDir:=apppath+'brushes';
  if SaveDialogAsBrush.Execute then begin
     terrain.saveMapAsBrush(SaveDialogAsBrush.FileName);

  end;
end;

procedure TtopPanel.MenuItemSaveAsClick(Sender: TObject);
begin
  saveMapAsBtn.Click;
end;

procedure TtopPanel.MenuItemSaveMapClick(Sender: TObject);
begin
  saveMapBtn.Click;
end;

procedure TtopPanel.menuNewMapClick(Sender: TObject);
begin
  toolbarNewMap.ShowModal;
end;

procedure TtopPanel.menuSetLogicPathClick(Sender: TObject);
begin
  if SelectDirectory.execute then logicEditorPath.text:=SelectDirectory.FileName;
end;

procedure TtopPanel.oxygenEmitterBtnClick(Sender: TObject);
begin
  editor.state:=esOxygenEmitter;
  ui.bringtofront;
end;

procedure TtopPanel.oxygnRenderBtnClick(Sender: TObject);
begin
  if btOxy1 in mineralsDontRender then mineralsDontRender:=mineralsDontRender-mineralsOxygen
  else begin
       mineralsDontRender:=mineralsDontRender+mineralsOxygen;
       terrain.cullAllChunks;
  end;
  ui.BringToFront;
end;

procedure TtopPanel.plantsToolbrBtnClick(Sender: TObject);
begin
  toolbarPlantGen.Show;
 // ui.BringToFront;
end;

procedure TtopPanel.remoteBrainEnabledClick(Sender: TObject);
begin
  if remoteBrainEnabled.Down then begin
     remoteBrainDebugging:=true;
     logic.skipUpdate:=false;
     logic.resetBrains;
  end else begin
     remoteBrainDebugging:=false;
     logic.skipUpdate:=true;
   //  remoteBrainEnabled.Down:=false;
  end;
  ui.BringToFront;
//  new game?
end;

procedure TtopPanel.saveActorsClick(Sender: TObject);
begin
  logic.saveActorsToFile(terrain.mapFileName);
end;

procedure TtopPanel.saveMapAsBtnClick(Sender: TObject);
begin
  if saveMapDlg.Execute then begin
     terrain.saveMap(saveMapDlg.FileName);
     clearUndoHistory;
  end;
end;

end.

