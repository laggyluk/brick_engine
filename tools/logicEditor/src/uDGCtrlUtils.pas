unit uDGCtrlUtils;

interface

uses
    SysUtils,
    fmx.Controls,
    Classes,
    fmx.Forms,
    IniFiles;

// this is the array of properties, the names are case sensitive
const CONTROL_PROPS: array[0..4] of string =
      ('Left', 'Top', 'Width', 'Height', 'Visible');

(*
you can also add more properties like Caption
increase the length of array by 1 from 4 to 5

const CONTROL_PROPS: array[0..5] of string =
      ('Left', 'Top', 'Width', 'Height', 'Visible', 'Caption');
and add Caption in the array
*)


  procedure SaveControls(toIniFile: TIniFile; fromForm: TForm);
  procedure LoadControls(fromIniFIle: TIniFile; toForm: TForm);
  procedure SaveControlsToFile(const FileName: string; fromForm: TForm);
  procedure LoadControlsFromFile(const FileName: string; toForm: TForm);

implementation

uses TypInfo;

procedure SaveControls(toIniFile: TIniFile; fromForm: TForm);
var i, j : integer;
    obj : TComponent;
    s, sec : string;
begin
     // store the section
     sec := fromForm.Name;
     // for each component on form
     for i := 0 to fromForm.ComponentCount -1 do begin
         // get it's reference into obj
         obj := fromForm.Components[i];
         // for each property defined in array
         for j := Low(CONTROL_PROPS) to High(CONTROL_PROPS) do
             // check if component has that property using RTTI
             if IsPublishedProp(obj, CONTROL_PROPS[j]) then begin
                // format the string ComponentName.Property
                s := Format('%s.%s', [obj.Name, CONTROL_PROPS[j]]);
                // write data to ini file
                toIniFile.WriteString(sec, s, GetPropValue(obj, CONTROL_PROPS[j]));
             end;// if IsPublishedProp(obj, CONTROL_PROPS[j]) then begin
     end;// for i := 0 to fromForm.ComponentCount -1 do begin
end;

procedure LoadControls(fromIniFIle: TIniFile; toForm: TForm);
var i, j : integer;
    obj : TComponent;
    s, sec, value : string;
begin
     // store the section
     sec := toForm.Name;
     // for each component on form
     for i := 0 to toForm.ComponentCount -1 do begin
         // get it's reference into obj
         obj := toForm.Components[i];
         // for each property defined in array
         for j := Low(CONTROL_PROPS) to High(CONTROL_PROPS) do
             // check if component has that property using RTTI
             if IsPublishedProp(obj, CONTROL_PROPS[j]) then begin
                // format the string ComponentName.Property
                s := Format('%s.%s', [obj.Name, CONTROL_PROPS[j]]);
                // read data from ini file
                value := fromIniFIle.ReadString(sec, s, EmptyStr);
                // check if value is not '' (EmptyStr)
                if value <> EmptyStr then
                   // set the property
                   SetPropValue(obj, CONTROL_PROPS[j], value);
             end;// if IsPublishedProp(obj, CONTROL_PROPS[j]) then begin
     end;// for i := 0 to fromForm.ComponentCount -1 do begin
end;

procedure SaveControlsToFile(const FileName: string; fromForm: TForm);
var ini : TIniFile;
begin
     ini := TIniFile.Create(FileName);
     SaveControls(ini, fromForm);
     FreeAndNil(ini);
end;

procedure LoadControlsFromFile(const FileName: string; toForm: TForm);
var ini : TIniFile;
begin
     ini := TIniFile.Create(FileName);
     LoadControls(ini, toForm);
     FreeAndNil(ini);
end;

end.