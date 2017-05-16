{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bmpLabelPackage;

interface

uses
  BitmapLabel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BitmapLabel', @BitmapLabel.Register);
end;

initialization
  RegisterPackage('bmpLabelPackage', @Register);
end.
