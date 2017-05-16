unit toolbar_brushes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ShellCtrls, core_types, core_editor, VectorGeometry, core_render,
  core_block_brush, VectorTypes, core_utils, core_chunk, core_terrain_generator,
  ComCtrls, Menus,menu_brush_box, core_editor_states, brickUImain,blocksToolbarUnit;

type

  //let brush be a list of block coordinates and type

  { TtoolbarBrushes }

  TtoolbarBrushes = class(TForm)
    addNewBrushBtn: TBitBtn;
    boxBtn: TButton;
    fillBtn: TButton;
    clearBtn: TButton;
    dir: TShellTreeView;
    EditBtn: TToggleBox;
    dirMenu: TPopupMenu;
    editBrushNameMnuItem: TMenuItem;
    deleteMenu: TMenuItem;
    procedure addNewBrushBtnClick(Sender: TObject);
    procedure boxBtnClick(Sender: TObject);
    procedure clearBtnClick(Sender: TObject);
    procedure dirMenuPopup(Sender: TObject);
    procedure dirMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditBtnChange(Sender: TObject);
    procedure EditBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure fillBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editBrushNameMnuItemClick(Sender: TObject);
    procedure deleteMenuClick(Sender: TObject);
  private
    { private declarations }
    //no exception when editing brush name please
    busy:boolean;
  public
    { public declarations }
  end;

var
  toolbarBrushes: TtoolbarBrushes;


implementation

{$R *.lfm}

{ TtoolbarBrushes }

procedure TtoolbarBrushes.EditBtnChange(Sender: TObject);
var
  ch:TVector2i;
  pos:TAffineVector;
  blk:tvector3b;
  i:integer;
begin
  clearbtn.Enabled:=editbtn.Checked;
  boxbtn.Enabled:=clearbtn.Enabled;
  if editbtn.Checked then editor.state:=esEditBrush else begin
     editor.state:=esSelectMode;
     renderer.blockBrush.saveAs(dir.GetPathFromNode(dir.Selected));
     //delete blocks added when creating brush
     //but what when block was created in some place and  later there was geometry created there? maybe if brush was created at 0,0,0
     {
     for i:=0 to length(blockBrush.data)-1 do if blockBrush.Data[i].typ<>btNone then
     begin
       blk:=absolutePositionToChunkBlok(ch,blockBrush.data[i].position);
       chunksPointer^[ch[0],ch[1]].setBlock(blk,btNone);
       terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
     end;}
     //enable brush follow cursor
     //renderer.gizmos.brush:=true;
     editor.state:=esPaintWithBrush;
  end;
end;

procedure TtoolbarBrushes.EditBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ui.BringToFront;
end;

procedure TtoolbarBrushes.fillBtnClick(Sender: TObject);
begin
  editor.state:=esPickBlockColor;
  toolbarBlocks.vibrate;
  //renderer.blockBrush.fill(editor.brushBlockType);
  ui.BringToFront;
end;

procedure TtoolbarBrushes.addNewBrushBtnClick(Sender: TObject);
var
  s,brushName:string;
  i:integer;

begin
  //create new filee
  s:=dir.GetSelectedNodePath;
  //check if file already exists. co no 2 brushes can have same name even in diferrent dir
  i:=1;
  while dir.Items.FindNodeWithText('new brush'+inttostr(i))<>nil do inc(i);
  brushName:='new brush'+ inttostr(i);
  //brush is selected so add new brush to it's parent rather than to brush node
  if fileexists(s) then begin
       dir.Items.AddChild(dir.Selected.Parent,brushName);
       fileclose(filecreate(dir.GetPathFromNode(dir.Selected.Parent) +'\'+brushName));
     end else begin
       dir.Items.AddChild(dir.Selected,brushName);//jezeli plik nie istnieje to mam nadzieje ze to folder jest selected node
       fileclose(filecreate(dir.GetSelectedNodePath+'\'+brushName));
     end;

  ui.BringToFront;
end;

//sets brush to box shape
procedure TtoolbarBrushes.boxBtnClick(Sender: TObject);
begin
  if menuBrushBox.Showmodal=mrOk then begin
     renderer.blockBrush.Box(menuBrushBox.axisx.value,menuBrushBox.axisy.Value,
                    menuBrushBox.axisz.Value,editor.brushBlockType);
     editbtn.Checked:=false;
  end;
  ui.BringToFront;
end;

procedure TtoolbarBrushes.clearBtnClick(Sender: TObject);
begin
  renderer.blockBrush.clear;
  ui.BringToFront;
end;

procedure TtoolbarBrushes.dirMenuPopup(Sender: TObject);
begin
  //don't allow deleting folders
  deleteMenu.Enabled:=fileexists(dir.GetPathFromNode(dir.Selected));
end;

procedure TtoolbarBrushes.dirMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button=mbright then dirmenu.PopUp
  else if (dir.Selected<>nil) and (fileexists(dir.GetPathFromNode(dir.Selected)))then
  begin
     //enable edit button
     editbtn.Enabled:=true;

     ui.BringToFront;
     //if brush is 0 sized then i't new file and don't try to load it from disk.
     if filesize(dir.GetPathFromNode(dir.Selected))=0 then exit;
     //load brush from file and enable paint brush mode
     renderer.blockBrush.load(dir.GetPathFromNode(dir.Selected));
     editor.state:=esPaintWithBrush;
  end;
end;

procedure TtoolbarBrushes.FormCreate(Sender: TObject);
begin
 // renderer.blockBrush:=TBlockBrush.create;
  dir.Root:=appPath+'brushes';
end;

procedure TtoolbarBrushes.FormDestroy(Sender: TObject);
begin
//  rendeblockBrush.Destroy;
end;

procedure TtoolbarBrushes.editBrushNameMnuItemClick(Sender: TObject);
var
  s,newName,oldName:string;
begin
  s:=inputbox('Rename to:','','');
  if s<>'' then begin
  //first check if this name already exists on the list
     oldName:=dir.GetSelectedNodePath;
     if dir.Items.FindNodeWithText(s)=nil then begin
        dir.Selected.Text:=s;
        //and then rename file
        //folder?
        newName:=dir.GetSelectedNodePath;
        renamefile(oldName,newName);
        end else
            showmessage('Name already exists');
  end;
  ui.BringToFront;
end;

procedure TtoolbarBrushes.deleteMenuClick(Sender: TObject);
var
  oldName:string;
begin
  if fileexists(dir.GetPathFromNode(dir.Selected))then
     if MessageDlg('Delete? Please confirm.',mtConfirmation, mbOKCancel, 0)=mrOk then
     begin
        oldName:=dir.GetSelectedNodePath;
        deletefile(oldName);
        dir.Items.Delete(dir.Items.FindNodeWithText(extractfilename(oldName)));
     end;
end;

end.

