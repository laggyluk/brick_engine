unit map_properties_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TmapProperties }

  TmapProperties = class(TForm)
    Button1: TButton;
    Button2: TButton;
    description: TMemo;
    Label1: TLabel;
    maxPlayers: TEdit;
    mapName: TEdit;
    UpDown1: TUpDown;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  mapProperties: TmapProperties;

implementation

{$R *.lfm}

end.

