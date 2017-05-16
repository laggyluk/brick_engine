unit core_matrixStack;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,dglOpenGL,VectorGeometry,core_types;
type
  { TMatrixStack }

  TMatrixStack = class
    top:PGLFloat;
    procedure pop;
    procedure push(m:PMatrix);
    constructor create;
   private
    data:array of TMatrix;
    index:integer;
    const maxSize = 256;
  end;

var
  matrixStack:TMatrixStack;//global

implementation

{ TMatrixStack }

procedure TMatrixStack.pop;
begin
  if index>0 then begin
    index:=index-1;
    //data[index]:=m;
  end else log('TMatrixStack.pop: can''t pop, stack base reached');
end;

procedure TMatrixStack.push(m: PMatrix);
begin
  if index<maxSize then begin
    index:=index+1;
    if index>=length(data) then setlength(data,index+1);
    MatrixMultiply(data[index-1],m^,data[index]);
    top:=@data[index];
  end else log('TMatrixStack.push: can''t push, stack maxSize reached');
end;

constructor TMatrixStack.create;
begin
  setlength(data,1);
  index:=0;
  data[0]:=IdentityHmgMatrix;
end;

end.

