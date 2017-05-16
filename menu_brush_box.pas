unit menu_brush_box;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls,core_block_brush;

type

  { TmenuBrushBox }

  TmenuBrushBox = class(TForm)
    axisx: TSpinEdit;
    axisz: TSpinEdit;
    axisy: TSpinEdit;
    okBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    okBtn1: TButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  menuBrushBox: TmenuBrushBox;

implementation

{$R *.lfm}

end.

