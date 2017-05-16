unit core_shader_program;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,core_types;

type

  { TProgram }

  TProgram = class
   ID:gluint;
   shaderName:string;
   resizeDirty:boolean;
   //wrapper around glGetUniformLocation, just makes sure uniform exists in shader code
   //actually prog is not used!
   function getUniformLocation(prog: glint; unifName: string):glint;
   function getUniformLocation(unifName: string):glint;overload;
   //usually view matrix uniform needs update on resize
   procedure resize;
   destructor destroy;
  protected
   procedure use;virtual;abstract;
   procedure init(prg:gluint);virtual;abstract;
  end;

implementation


function tprogram.getUniformLocation(prog: glint; unifName: string):glint;
begin
  result:=glGetUniformLocation(id,pchar(unifName));
  if result=-1 then log('Achtung! uniform not found: '+unifName+ ' in '+shaderName);
end;

function TProgram.getUniformLocation(unifName: string): glint;
begin
  result:=glGetUniformLocation(id,pchar(unifName));
  if result=-1 then log('Achtung! uniform not found: '+unifName+ ' in '+shaderName);
end;

destructor TProgram.destroy;
begin
  glDeleteProgram(id);
end;

procedure TProgram.resize;
begin
  resizeDirty:=true;
end;

end.

