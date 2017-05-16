unit BitmapLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

const
  sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
               {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
type

  { TBitmapLabel }

  TBitmapLabel = class(TImage)
  private
    { Private declarations }
  protected
    { Protected declarations }
    fCaption:TStrings;
    fFontSize:integer;
    fFontColor:TColor;
    fbgColor:TColor;
    procedure setCaption(const AValue: TStrings);
  public
    { Public declarations }
    constructor create(aOwner: TComponent);override;
    destructor destroy;override;
  published
    { Published declarations }
    property fontColor:TColor read fFontColor write fFontColor;
    property txt:TStrings read fCaption write setCaption;
    property FontSize:integer read fFontSize write fFontSize;
    property bgColor:TColor read fbgColor write fbgColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I bitmaplabel_icon.lrs}
  RegisterComponents('Additional',[TBitmapLabel]);
end;

{ TBitmapLabel }

procedure TBitmapLabel.setCaption(const AValue: TStrings);
begin
  if (aValue <> nil) then
    fcaption.Assign(aValue);
end;

constructor TBitmapLabel.create(aOwner:TComponent);
begin
  inherited create(aowner);
  fontSize:=14;
  fontColor:=clNone;
  fcaption:=TStringList.create;
  bgColor:=$000C0C0C;
end;

destructor TBitmapLabel.destroy;
begin
  txt.free;
  inherited destroy;
end;

end.
