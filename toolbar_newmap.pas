unit toolbar_newMap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls,
  core_terrain_generator,core_main, brickUImain;

type

  { TtoolbarNewMap }

  TtoolbarNewMap = class(TForm)
    cancelBtn: TButton;
    newMapBtn: TButton;
    Button3: TButton;
    Button4: TButton;
    empty: TCheckBox;
    chunkH: TEdit;
    chunkW: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mapH: TEdit;
    mapW: TEdit;
    procedure newMapBtnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  toolbarNewMap: TtoolbarNewMap;

implementation

{$R *.lfm}

{ TtoolbarNewMap }

procedure TtoolbarNewMap.newMapBtnClick(Sender: TObject);
begin
  //prepare
  core.setMapSize(strtoint(chunkw.Text) ,strtoint(chunkH.Text),
                  strtoint(mapW.Text),strtoint(maph.Text));
  terrain.initMap(empty.Checked);
  close;
  ui.BringToFront;
end;

procedure TtoolbarNewMap.Button3Click(Sender: TObject);
begin
  mapw.Text:='4';
  maph.Text:='4';
  chunkH.Text:='128';
end;

procedure TtoolbarNewMap.Button4Click(Sender: TObject);
begin
  mapw.Text:='1';
  maph.Text:='1';
  chunkH.Text:='64';
end;

end.

