unit ui_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    ui_menu_template, BitmapLabel, core_ui;

type

  { TmenuMap }

  TmenuMap = class(TmenuTemplate)
    BitmapLabel3: TBitmapLabel;
    planetShape: TShape;
    windowNameLabel: TBitmapLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  menuMap: TmenuMap;

implementation

{$R *.lfm}

{ TmenuMap }

procedure TmenuMap.FormCreate(Sender: TObject);
begin
  inherited;
  lockHeight:=true;
  lockWidth:=true;
  planetSHape.Pen.Color:=fontColorGreen;
  planetShape.brush.Color:=backgroundColorGrey;
end;

end.

