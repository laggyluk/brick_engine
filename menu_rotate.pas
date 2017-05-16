unit menu_rotate;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, core_utils;

type

  { TmenuRotate }

  TmenuRotate = class(TForm)
    xBar: TTrackBar;
    zBar: TTrackBar;
    yBar: TTrackBar;
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowAtCursor;
  end;

var
  menuRotate: TmenuRotate;

implementation

{$R *.lfm}

procedure TmenuRotate.ShowAtCursor;
var
  p:tpoint;
begin
  p:=getMousePosition;

  left:=p.x-width div 2;
  top:=p.y+100;
  show;
end;

end.

