unit toolbar_game_objs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  Buttons, core_types, ComCtrls,brickUImain;

type

  { TtoolbarGameObjs }

  TtoolbarGameObjs = class(TForm)
    addNewObj: TBitBtn;
    dir: TShellTreeView;
    procedure addNewObjClick(Sender: TObject);
    procedure dirChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  toolbarGameObjs: TtoolbarGameObjs;

implementation

{$R *.lfm}

{ TtoolbarGameObjs }

procedure TtoolbarGameObjs.FormCreate(Sender: TObject);
begin
  dir.Root:=apppath+'objects';
end;

procedure TtoolbarGameObjs.addNewObjClick(Sender: TObject);
var
  s,objName:string;
  i:integer;
begin
  //create new filee
  s:=dir.GetSelectedNodePath;
  //check if file already exists. co no 2 brushes can have same name even in diferrent dir
  i:=1;
  while dir.Items.FindNodeWithText('new object'+inttostr(i))<>nil do inc(i);
  objName:='new object'+ inttostr(i);
  //brush is selected so add new brush to it's parent rather than to brush node
  if fileexists(s) then begin
       dir.Items.AddChild(dir.Selected.Parent,objName);
       filecreate(dir.GetPathFromNode(dir.Selected.Parent) +'\'+objName);
     end else begin
       dir.Items.AddChild(dir.Selected,objName);//jezeli plik nie istnieje to mam nadzieje ze to folder jest selected node
       filecreate(dir.GetSelectedNodePath+'\'+objName);
     end;
  ui.BringToFront;
end;

procedure TtoolbarGameObjs.dirChange(Sender: TObject; Node: TTreeNode);
begin

end;

end.

