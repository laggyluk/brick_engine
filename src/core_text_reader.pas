unit core_text_reader;

{$mode delphi}{$H+}
//simple text file encryption
//use default crypt key: 8975
interface

uses
  Classes, SysUtils, dialogs ;

  procedure loadTextFile(loadTo:tStrings;const path:string;key:word = 8975);
  procedure saveTextFile(lines:TStrings;const path:string;key:word = 8975);

implementation

// simple string encryption-------------------------------
const CKEY1 = 53721;
      CKEY2 = 36638;

function EnStr(const S :WideString; Key: Word=8975): String;
var   i          :Integer;
      RStr       :ansistring;
      RStrB      :TBytes Absolute RStr;
begin
  Result:= '';
  RStr:= UTF8Encode(S);
  for i := 0 to Length(RStr)-1 do begin
    RStrB[i] := RStrB[i] xor (Key shr 8);
    Key := (RStrB[i] + Key) * CKEY1 + CKEY2;
  end;
  for i := 0 to Length(RStr)-1 do begin
    Result:= Result + IntToHex(RStrB[i], 2);
  end;
end;

function DeStr(const S: String; Key: Word = 8975): String;
var   i, tmpKey  :Integer;
      RStr       :ansistring;
      RStrB      :TBytes Absolute RStr;
      tmpStr     :string;
begin
  tmpStr:= UpperCase(S);
  SetLength(RStr, Length(tmpStr) div 2);
  i:= 1;
  try
    while (i < Length(tmpStr)) do begin
      RStrB[i div 2]:= StrToInt('$' + tmpStr[i] + tmpStr[i+1]);
      Inc(i, 2);
    end;
  except
    Result:= '';
    Exit;
  end;
  for i := 0 to Length(RStr)-1 do begin
    tmpKey:= RStrB[i];
    RStrB[i] := RStrB[i] xor (Key shr 8);
    Key := (tmpKey + Key) * CKEY1 + CKEY2;
  end;
  Result:= UTF8Decode(RStr);
end;

procedure loadTextFile(loadTo: tStrings; const path: string; key: word);
var i:integer;
begin
  try
  loadto.LoadFromFile(path);
  for i:=0 to loadTo.Count-1 do loadTo[i]:=DeStr(loadTo[i],key);

  finally
    //showmessage('exception: are you sure you opened crypted file?');
  end;
end;

procedure saveTextFile(lines: TStrings; const path: string; key: word);
var i:integer;
    str:tstrings;
begin
  try
  str:=tstringlist.create;
  str.Text:=lines.Text;
  for i:=0 to str.Count-1 do str[i]:=enStr(str[i],key);
  str.savetoFile(path);
  finally
    str.free;
  end;
end;

// simple string encryption-------------------------------

end.

