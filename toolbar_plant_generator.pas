unit toolbar_plant_generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  Buttons, ShellCtrls, Menus,core_types, core_editor_states,
  blocksToolbarUnit,core_plant_generator, core_utils, core_editor;

type

  { TtoolbarPlantGen }

  TtoolbarPlantGen = class(TForm)
    angleX1: TEdit;
    angleY1: TEdit;
    generateBtn: TButton;
    Button2: TButton;
    iterations: TEdit;
    formulaFile: TEdit;
    initAngleX: TEdit;
    initAngleY: TEdit;
    initAngleZ: TEdit;
    axiom: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    blocks: TListBox;
    menuDelete: TMenuItem;
    treePopup: TPopupMenu;
    rulesFrom: TMemo;
    saveBtn: TSpeedButton;
    tree: TShellTreeView;
    sizeX: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rulesTo: TMemo;
    angleY: TEdit;
    angleZ: TEdit;
    sizeY: TEdit;
    sizeZ: TEdit;
    angleX: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure generateBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure menuDeleteClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
    procedure treeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure treeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    procedure refreshTree;
    procedure saveToFile(const filename:string);
  public
    { public declarations }
  end;

var
  toolbarPlantGen: TtoolbarPlantGen;

implementation

{$R *.lfm}

{ TtoolbarPlantGen }

procedure TtoolbarPlantGen.SpeedButton1Click(Sender: TObject);
begin
  if blocks.Items.IndexOf(toolbarBlocks.colorList.items[toolbarBlocks.colorList.ItemIndex])>-1 then exit;
  blocks.AddItem(toolbarBlocks.colorList.items[toolbarBlocks.colorList.ItemIndex],nil);
end;

procedure TtoolbarPlantGen.FormCreate(Sender: TObject);
begin
  tree.Root:=apppath+'LSystem';
end;

procedure TtoolbarPlantGen.menuDeleteClick(Sender: TObject);
begin
  if MessageDlg('Sure?',mtConfirmation, mbOKCancel, 0)=mrOk then begin
     if fileExists(TREE.GetSelectedNodePath) then deletefile(tree.GetSelectedNodePath);
     refreshTree;
  end;
end;

procedure TtoolbarPlantGen.FormActivate(Sender: TObject);
begin
  refreshTree;
end;

procedure TtoolbarPlantGen.Button2Click(Sender: TObject);
begin
  rulesFrom.Clear;
  rulesTo.Clear;
  blocks.Clear;
end;

procedure TtoolbarPlantGen.generateBtnClick(Sender: TObject);
begin
  savetofile(tree.root+'\temp');
  plantGenerator.fromFile(1,1,tree.root+'\temp',true);
  editor.state:=esPaintWithBrush;
  //ui.bringtofront;
end;

procedure TtoolbarPlantGen.SpeedButton2Click(Sender: TObject);
begin
  if blocks.ItemIndex>-1 then
      blocks.Items.Delete(blocks.ItemIndex);
end;

procedure TtoolbarPlantGen.saveBtnClick(Sender: TObject);
begin
  if formulaFile.Text='' then begin
     showmessage('Set filename first');
  end;
  if ExtractFileExt(formulafile.Text)<>'.txt' then formulaFile.Text:=formulaFile.Text+'.txt';
  savetofile(tree.root+'\'+formulaFile.Text);
  refreshTree;
end;

procedure TtoolbarPlantGen.treeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button=TMouseButton.mbRight then treePopup.PopUp;
end;

procedure TtoolbarPlantGen.treeSelectionChanged(Sender: TObject);
var s,s1,s2:string;
    list:tstringlist;
begin
  if tree.Selected=nil then exit;
  formulafile.Text:=tree.Selected.Text;
  if ExtractFileExt(formulafile.Text)='.txt' then begin
     rulesFrom.Clear;
     rulesTo.Clear;
     blocks.Clear;
     //load values to controls
     list:=tstringlist.create;
     s:=tree.root+'\'+tree.Selected.GetTextPath;
     list.LoadFromFile(s);
     for s2 in list do begin
        s:=s2;
        s1:=eatstring(s);
        if s1='rule' then begin
           rulesFrom.Lines.add(eatstring(s));
           rulesTo.Lines.add(eatstring(s));
           continue;
        end;
        if s1='axiom' then axiom.text:=eatstring(s);
        if s1='iterations' then iterations.text:=eatstring(s);
        if s1='size' then begin
           sizex.text:=eatstring(s);
           sizey.text:=eatstring(s);
           sizez.text:=eatstring(s);
        end;
        if s1='angle' then begin
           anglex.text:=eatstring(s);
           angley.text:=eatstring(s);
           anglez.text:=eatstring(s);
        end;
        if s1='initialAngle' then begin
           initAnglex.text:=eatstring(s);
           initAngley.text:=eatstring(s);
           initAnglez.text:=eatstring(s);
        end;
        if s1='colors' then begin
           s1:=eatstring(s);
           while length(s1)>2 do begin
              blocks.Items.add(s1);
              s1:=eatstring(s);
           end;
        end;
     end;
     list.free;
  end else formulafile.text:='';
end;

procedure TtoolbarPlantGen.refreshTree;
var s:string;
begin
  s:=tree.Root;
  tree.BeginUpdate;
  tree.Root:='c:\';
  tree.root:=s;
  tree.EndUpdate;
end;

procedure TtoolbarPlantGen.saveToFile(const filename: string);
var s,s1:string;
    list:tstringlist;
    i:integer;
begin
  if rulesFrom.lines.count<>rulesTo.lines.count then begin
     showmessage('rules line count doesn''t match');
     exit;
  end;
  list:=tstringlist.create;
  for i:=0 to rulesFrom.lines.count-1 do list.Add('rule;'+rulesFrom.lines[i]+';'+rulesTo.lines[i]+';');
  list.add('axiom;'+axiom.text+';');
  list.add('iterations;'+iterations.text+';');
  list.add('size;'+sizeX.text+';'+sizeY.text+';'+sizeZ.text+';');
  list.add('angle;'+angleX.text+';'+angleY.text+';'+angleZ.text+';');
  list.add('initialAngle;'+initAngleX.text+';'+initAngleY.text+';'+initAngleZ.text+';');
  s1:='colors;';
  for i:=0 to blocks.Items.count-1 do s1:=s1+blocks.Items[i]+';';
  list.add(s1);
  list.SaveToFile(filename);
  list.free;
  tree.Refresh;
end;

end.

