unit toolbar_flat_units;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls,core_editor,
  ComCtrls, core_render, brickUImain;

type

  { TtoolbarFlats }

  TtoolbarFlats = class(TForm)
    tree: TTreeView;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure treeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
    //selectedUnitType:integer;
    //fills up the list with entries from units def file/atlas?
    procedure populate;
  end;

var
  toolbarFlats: TtoolbarFlats;


implementation

{$R *.lfm}

{ TtoolbarFlats }

procedure TtoolbarFlats.FormActivate(Sender: TObject);
begin
  if tree.Items.count=0 then  populate;
end;

procedure TtoolbarFlats.FormCreate(Sender: TObject);
begin
  //editor.selectedUnitType:=0;
end;

procedure TtoolbarFlats.treeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if tree.Selected<>nil then begin
     editor.selectedUnitType:=tree.Selected.AbsoluteIndex;
     //editor.state:=esActor;
     ui.FocusFromToolbar('toolbarFlats');
  end
  else ui.BringToFront;
end;

procedure TtoolbarFlats.populate;
var
  i:integer;
begin
  tree.Items.Clear;
  //fill with names from atlas. indexes will fit
  for i:=0 to length(renderer.unitsAtlas.data)-1 do
      tree.Items.add(nil,renderer.unitsAtlas.data[i].name);
end;

end.

