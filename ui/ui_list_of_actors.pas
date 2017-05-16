unit ui_list_of_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, core_orders , core_orders_manager, core_types, core_logic,
  core_behaviour_tree, core_main;

type

  { TactorsListForm }

  TactorsListForm = class(TForm)
    grid: TStringGrid;
    deleteActorBtn: TBitBtn;
    refreshGrid: TBitBtn;
    refreshGrid1: TBitBtn;
    setCamera: TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure deleteActorBtnClick(Sender: TObject);
    procedure gridDblClick(Sender: TObject);
    procedure refreshGrid1Click(Sender: TObject);
    procedure refreshGridClick(Sender: TObject);
    procedure setCameraClick(Sender: TObject);
  private
    { private declarations }
    procedure populateGrid;
    function selectedActorID:integer;
  public
    { public declarations }

  end;

var
  actorsListForm: TactorsListForm;

implementation

{$R *.lfm}

{ TactorsListForm }

procedure TactorsListForm.FormActivate(Sender: TObject);
begin
  populateGrid;
end;

procedure TactorsListForm.deleteActorBtnClick(Sender: TObject);
begin
  if MessageDlg('Delete actor?',mtConfirmation, mbOKCancel, 0)=mrOk then begin
     om.addorder(orDestroyActor,strtoint(grid.cells[0,grid.row]),0);
     grid.DeleteRow(grid.Row);
     //populateGrid;//actually this won't work till next frame
  end;
end;

procedure TactorsListForm.gridDblClick(Sender: TObject);
begin
  setCamera.click;
end;

procedure TactorsListForm.refreshGrid1Click(Sender: TObject);
begin
  setRemoteBrain(selectedActorID);
end;

procedure TactorsListForm.refreshGridClick(Sender: TObject);
begin
  populateGrid;
end;

procedure TactorsListForm.setCameraClick(Sender: TObject);
begin
  //set cam to lock on actor
  om.addOrder(orCameraMode,selectedActorID,integer(cmFollow));
  //update everything and cam position too
  core.update(1);
  //release cam again
  om.addOrder(orCameraMode,selectedActorID,integer(cmFree));
  //ui.BringToFront;
end;

procedure TactorsListForm.populateGrid;
var i:integer;
begin
  grid.Clear;
  grid.RowCount:=logic.actors.count+1;
  grid.Cells[0,0]:='actorID';
  grid.Cells[1,0]:='asset name';
  for i:=0 to logic.actors.count-1 do with logic.actors.data[i] do begin
    grid.cells[0,i+1]:=inttostr(actorID);
    grid.cells[1,i+1]:=common.assetName;
  end;
end;

function TactorsListForm.selectedActorID: integer;
begin
  result:=strtoint(grid.cells[0,grid.row])
end;

end.

