unit toolbar_terrain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, inifiles,
  StdCtrls, Buttons, core_terrain_generator, PerlinNoise, core_types, core_utils,
  brickUImain;

type

  { TtoolbarTerra }

  TtoolbarTerra = class(TForm)
    Button1: TButton;
    Button2: TButton;
    randomizeAllBtn: TButton;
    comboMineral1: TComboBox;
    comboMineral2: TComboBox;
    comboMineral3: TComboBox;
    comboMineral4: TComboBox;
    items: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    loadMapBtn: TBitBtn;
    saveMapBtn: TBitBtn;
    templateFile: TEdit;
    seed: TEdit;
    hangovers: TTrackBar;
    Label1: TLabel;
    scalel: TLabel;
    flatnessL: TLabel;
    cutoffL: TLabel;
    flatness: TTrackBar;
    cutoff: TTrackBar;
    scale: TTrackBar;
    hangoversL: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cutoffChange(Sender: TObject);
    procedure flatnessChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hangoversChange(Sender: TObject);
    procedure itemsClick(Sender: TObject);
    procedure loadMapBtnClick(Sender: TObject);
    procedure randomizeAllBtnClick(Sender: TObject);
    procedure saveMapBtnClick(Sender: TObject);
    procedure scaleChange(Sender: TObject);
  private
    { private declarations }
    procedure randomizeSettings;
  public
    { public declarations }
    procedure populateList;
  end;

var
  toolbarTerra: TtoolbarTerra;

implementation

{$R *.lfm}

{ TtoolbarTerra }

procedure TtoolbarTerra.Button1Click(Sender: TObject);
begin
  perlin3dnoise.Initialize(strtoint(seed.Text));
  terrain.generateMap(flatness.Position,cutoff.Position,scale.position/100,hangovers.Position/100,
          eBlockTypes(comboMineral1.ItemIndex),eBlockTypes(comboMineral2.ItemIndex),
          eBlockTypes(comboMineral3.ItemIndex),eBlockTypes(comboMineral4.ItemIndex));
  ui.BringToFront;
end;

procedure TtoolbarTerra.Button2Click(Sender: TObject);
begin
  seed.Text:=inttostr(random(9999999));
end;

procedure TtoolbarTerra.cutoffChange(Sender: TObject);
begin
  cutoffl.Caption:='Cutoff: '+floattostr(cutoff.Position);
end;

procedure TtoolbarTerra.flatnessChange(Sender: TObject);
var wtf:integer;
begin
  wtf:=(flatness.Position);
  flatnessl.Caption:='Flatness: '+inttostr(wtf);
end;

procedure TtoolbarTerra.FormActivate(Sender: TObject);
begin

end;

procedure TtoolbarTerra.FormCreate(Sender: TObject);
var
  str:tstringlist;
  e:eBlockTypes;
begin
  str:=tstringlist.create;
  for e:=low(eBlockTypes) to high(eBlockTypes) do str.Add(mineralToStr(e));
  comboMineral1.Items.AddStrings(str);
  comboMineral2.Items.AddStrings(str);
  comboMineral3.Items.AddStrings(str);
  comboMineral4.Items.AddStrings(str);
  str.free;

  randomizeSettings;
end;

procedure TtoolbarTerra.hangoversChange(Sender: TObject);
begin
  hangoversl.Caption:='Noise: '+floattostr(hangovers.position/100);
end;

procedure TtoolbarTerra.itemsClick(Sender: TObject);
var
  ini:tinifile;
begin
  if items.ItemIndex>-1 then begin
    templateFile.Text:=items.Items[items.itemindex];
    ini:=tinifile.create(apppath+'terrain'+DirectorySeparator+'terrains.ini');
    flatness.Position:=ini.readInteger(templateFile.Text,'flatness',0);
    cutoff.Position:=ini.readInteger(templateFile.Text,'cutoff',0);
    scale.Position:=ini.readInteger(templateFile.Text,'xyscale',0);
    hangovers.Position:=ini.readInteger(templateFile.Text,'noise',0);
    ini.free;
  end;
end;

procedure TtoolbarTerra.loadMapBtnClick(Sender: TObject);
var
  ini:tinifile;
begin
  if items.ItemIndex>-1 then begin
    items.Items.Delete(items.ItemIndex);
    ini:=tinifile.create(apppath+'terrain'+DirectorySeparator+'terrains.ini');
    ini.EraseSection(templateFile.Text);
    ini.free;
    populateList;
  end;
end;

procedure TtoolbarTerra.randomizeAllBtnClick(Sender: TObject);
begin
  randomizeSettings;
end;

procedure TtoolbarTerra.saveMapBtnClick(Sender: TObject);
var
  ini:tinifile;
begin
  ini:=tinifile.create(apppath+'terrain'+DirectorySeparator+'terrains.ini');
  ini.WriteInteger(templateFile.Text,'flatness',flatness.Position);
  ini.WriteInteger(templateFile.Text,'cutoff',cutoff.Position);
  ini.WriteInteger(templateFile.Text,'xyscale',scale.Position);
  ini.WriteInteger(templateFile.Text,'noise',hangovers.Position);
  ini.free;
  populateList;
end;

procedure TtoolbarTerra.scaleChange(Sender: TObject);
begin
  scalel.Caption:='Scale: '+floattostr(scale.position/100);
end;

procedure TtoolbarTerra.randomizeSettings;
begin
  comboMineral1.ItemIndex:=random(integer(btMax)-2)+2;
  comboMineral2.ItemIndex:=random(integer(btMax)-2)+2;
  comboMineral3.ItemIndex:=random(integer(btMax)-2)+2;
  comboMineral4.ItemIndex:=random(integer(btMax)-2)+2;

  flatness.Position:=random(flatness.Max);
  cutoff.position:=random(chunk_height2 div 2) + chunk_height2;
  scale.Position:=random(scale.Max);
  hangovers.Position:=random(hangovers.max);

end;

procedure TtoolbarTerra.populateList;
var
  ini:tinifile;
  sections:tstringlist;
  i:integer;
begin
  items.clear;
  ini:=tinifile.create(apppath+'terrain'+DirectorySeparator+'terrains.ini');
  sections:=tstringlist.create;
  ini.ReadSections(sections);
  for i:=0 to sections.Count-1 do begin
      items.AddItem(sections[i],nil);
  end;
  ini.free;
  sections.free;
  //remove files with other extension
end;


end.

