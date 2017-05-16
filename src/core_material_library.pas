unit core_material_library;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,core_types,fgl,core_camera,VectorGeometry,
  core_forward_shaders,core_shader_program,core_defered_shaders;

type

  { TShaderSelectionCube }

  TShaderSelectionCube = class (TProgram)
    unifColor:glint;
    unifWorldToCameraMatrix:glint;
    unifCameraToClipMatrix:glint;
    unifModelToWorldMatrix:glint;
    procedure use;
    procedure init(prg:gluint);
    procedure resize;
  end;

  TProgramList = specialize TFPGMap<string,TProgram>;

  { TMaterialLib }

  TMaterialLib = class
     //i guess sekection cube can be used for both deferred and forward rendering
     shaderSelectionCube:TShaderSelectionCube;
     shaderChunkDS:TShaderChunkDS;
     shaderQuadDS:TShaderDirLightQuadDS;
     shaderCubeLightDS:TShaderPointLightDS;
     shaderChunkF:TShaderForwardLighting;
     shaderSSAO:TShaderSSAO;
     shaderNullDS:TNullDS;
     shaderFlatPpl:TShaderFlatDS;
     shaderUI:TShaderUI;
     function getProgram(shaderFileName:string):gluint;
     procedure resize;
     constructor create;
     destructor destroy;
     procedure init(deferred:boolean);
   private
     programs:TProgramList;  //holds pointers shader programs
     function CreateShader(eShaderType:GLenum; strShaderFile:string):GLuint;
     function CreateProgram(var shaderList: array of GLuint):GLuint;
  end;


var
   matLib:TMaterialLib;

implementation

{ TShaderSelectionCube }

procedure TShaderSelectionCube.use;
begin
    glUseProgram(id);
    glUniformMatrix4fv(unifWorldToCameraMatrix, 1, bytebool(GL_FALSE), @camera.WorldToCameraMatrix);
    if resizeDirty then begin
      //glUseProgram(id);
      glUniformMatrix4fv(unifCameraToClipMatrix, 1, bytebool(GL_FALSE), @camera.projectionMatrix);
      resizeDirty:=false;
    end;
end;

procedure TShaderSelectionCube.init(prg: gluint);
begin
    glUseProgram(prg);
    id:=prg;
    unifColor:=getUniformLocation(prg,'color');
    unifWorldToCameraMatrix:=GetUniformLocation(prg,'worldToCameraMatrix');
    unifCameraToClipMatrix:=GetUniformLocation(prg,'cameraToClipMatrix');
    unifModelToWorldMatrix:=GetUniformLocation(prg,'modelToWorldMatrix');
end;

procedure TShaderSelectionCube.resize;
begin
    resizeDirty:=true;
end;

procedure TMaterialLib.init(deferred:boolean);
begin
  shaderSelectionCube:=TShaderSelectionCube.Create;
  shaderSelectionCube.shaderName:='simple';
  shaderSelectionCube.init(matLib.getProgram('simple'));
  //shaders for deferred mode
  if deferred then begin
    shaderChunkDS:=TShaderChunkDS.create;
    //shaderChunkDS.shaderName:='dsChunkGeom';
    //shaderChunkDS.init(matLib.getProgram('dsChunkGeom'));
    shaderChunkDS.shaderName:='dsChunkStatic';
    shaderChunkDS.init(matLib.getProgram('dsChunkStatic'));

    shaderQuadDS:=TShaderDirLightQuadDS.Create;
    shaderQuadDS.shaderName:='dsDirLight';
    shaderQuadDS.init(matLib.getProgram('dsDirLight'));

    shaderCubeLightDS:=TShaderPointLightDS.Create;
    shaderCubeLightDS.shaderName:='dsCubeLight';
    shaderCubeLightDS.init(matLib.getProgram('dsCubeLight'));

    shaderNullDS:=TNullDS.Create;
    shaderNullDS.shaderName:='dsNull';
    shaderNullDS.init(matLib.getProgram('dsNull'));

    shaderSSAO:=TshaderSSAO.Create;
    shaderSSAO.shaderName:='dsSSAO';
    shaderSSAO.init(matLib.getProgram('dsSSAO'));

    shaderFlatPpl:=TShaderFlatDS.create;
    shaderFlatPpl.shaderName:='dsFlatPeople!';
    shaderFlatPpl.init(matLib.getProgram('dsFlat'));

    shaderUI:=TShaderUI.create;
    shaderUI.shaderName:='dsUI!';
    shaderUI.init(matLib.getProgram('dsUI'));

  end else begin
    shaderChunkF:=TShaderForwardLighting.Create;
    shaderChunkF.shaderName:='fChunkStatic';
    shaderChunkF.init(matLib.getProgram('fChunkStatic'));
  end;
end;

{ TMaterialLib }
function TMaterialLib.getProgram(shaderFileName: string): gluint;
var
   mat,i:integer;
   str:TStringlist;
   sType:glenum;
   shaderList:array of GLuint;
   s:string;
   prg:gluint;
   wtf:tprogram;
   u:glint;
begin
 result:=0;
 prg:=0;
 mat:=programs.IndexOf(shaderFileName);
 if mat>-1 then begin //material already in lib
   result:=programs[shaderFileName].ID;
   exit;
 end
 //material not found in lib so load shaders from disk
 else begin
    s:=appPath+'shaders\'+shaderFileName+'.vert';
    if fileexists(s) then
    begin
      setlength(shaderList,length(shaderList)+1);
      shaderList[length(shaderList)-1]:=CreateShader(GL_VERTEX_SHADER, s);
    end;// else log('TMaterialLibrary.getProgram: vert shader missing:'+shaderFileName);

    s:=appPath+'shaders\'+shaderFileName+'.frag';
   if fileexists(s) then
   begin
     setlength(shaderList,length(shaderList)+1);
     shaderList[length(shaderList)-1]:=CreateShader(GL_FRAGMENT_SHADER, s);
   end;// else log('TMaterialLibrary.getProgram: frag shader missing:'+shaderFileName);
   s:=appPath+'shaders\'+shaderFileName+'.geom';
   if fileexists(s) then
   begin
     setlength(shaderList,length(shaderList)+1);
     shaderList[length(shaderList)-1]:=CreateShader(GL_GEOMETRY_SHADER, s);
   end;// else log('TMaterialLibrary.getProgram: geom shader missing:'+shaderFileName);

   prg:=CreateProgram(shaderList);
   programs.add(shaderFileName);
   for i:=0 to high(shaderList) do glDeleteShader(shaderList[i]);
   //u:=GetUniformLocation(prg,'cameraToClipMatrix');
   wtf:=tprogram.create;
   wtf.ID:=prg;
   glUseProgram(prg);
   programs[shaderFileName]:=wtf;
   result:=programs[shaderFileName].ID;
 end;
end;

procedure TMaterialLib.resize;
begin
   { TODO -ccleanup : moze shadery powinny byc lista a nazwy wlasne byly by tylko referencjami do tej listy }
    shaderChunkDS.resize;
    shaderCubeLightDS.resize;
    shaderQuadDS.resize;
    shaderSSAO.resize;
    shaderFlatPpl.resize;
    shaderUI.resize;
    matlib.shaderSelectionCube.resize;
end;

constructor TMaterialLib.create;
var
   item:TStringList;
   s:string;
   i:integer;
begin
  programs:=TProgramList.create;
end;

destructor TMaterialLib.destroy;
var
   i:integer;
begin
  inherited;
  for i:=0 to programs.Count-1 do programs.Data[i].free;
  programs.Destroy;
  if shaderChunkDS<>nil then shaderChunkDS.destroy;
  if shaderCubeLightDS<>nil then shaderCubeLightDS.destroy;
  if shaderQuadDS<>nil then shaderQuadDS.destroy;
  if shaderSelectionCube<>nil then shaderSelectionCube.destroy;
  if shaderChunkF<>nil then shaderChunkF.destroy;
  if shaderNullDS<>nil then shaderNullDS.destroy;
  if shaderSSAO<>nil then shaderSSAO.destroy;
  if shaderFlatPpl<>nil then shaderFlatPpl.destroy;
  shaderUI.destroy;
end;

function TMaterialLib.CreateShader(eShaderType:GLenum; strShaderFile:string):GLuint;
var
   shader:GLuint;
   f:tstringList;
   status:GLint;
   infoLogLength:GLint;
   strInfoLog:PGLchar;
   g:pglsizei;
   s:string;
begin
     f:=tstringList.create();
     f.LoadFromFile(strShaderFile);
     shader := glCreateShader(eShaderType);
    // const char *strFileData = strShaderFile.c_str();
    // glShaderSource(shader, 1, &strFileData, NULL);
    s:=f.text;
    glShaderSource(shader, 1, PPGLChar(@s), nil);
    glCompileShader(shader);
    glGetShaderiv(shader, GL_COMPILE_STATUS, @status);
    if (status = 0) then
    begin
//        g:=0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, @infoLogLength);
        strInfoLog := allocMem (sizeof(GLcharARB)*infoLogLength + 1);
        glGetShaderInfoLog(shader, infoLogLength, g, strInfoLog);
        case(eShaderType) of
          GL_VERTEX_SHADER: s:= 'vertex ';
          GL_GEOMETRY_SHADER: s:= 'geometry ';
          GL_FRAGMENT_SHADER: s:= 'fragment ';
        end;
        log('Compile failure in shader: '+strShaderFile+', '+s+ strInfoLog);
        freemem(strInfoLog,infoLogLength);
    end;
    f.free;
    result:=shader;
end;

function TMaterialLib.CreateProgram(var shaderList: array of GLuint):GLuint;
var
   prog:GLuint;
   status:GLint;
   i:integer;
   infoLogLength:GLint;
   strInfoLog:PGLchar;
//   g:glint;
   g:pglsizei;
begin
    prog := glCreateProgram();
    for i:= 0 to high(shaderList) do
    	glAttachShader(prog, shaderList[i]);
    glLinkProgram(prog);
    glGetProgramiv (prog, GL_LINK_STATUS, @status);
    //if (status = GL_FALSE) then
    if (status = 0) then
    begin
      //g:=0;
      glGetProgramiv(prog, GL_INFO_LOG_LENGTH, @infoLogLength);
      strInfoLog := allocMem(sizeof(GLcharARB)*infoLogLength + 1);
      glGetProgramInfoLog(prog, infoLogLength, g, strInfoLog);
      log('shader linker failure: '+ strInfoLog);
      freeMem(strInfoLog);
    end;
    glValidateProgram(prog);
    glGetProgramiv (prog, GL_VALIDATE_STATUS, @status);
    if (status = 0) then
    begin
      //g:=0;
      glGetProgramiv(prog, GL_INFO_LOG_LENGTH, @infoLogLength);
      strInfoLog := allocMem(sizeof(GLcharARB)*infoLogLength + 1);
      glGetProgramInfoLog(prog, infoLogLength, g, strInfoLog);
      log('shader validate failure: '+ strInfoLog);
      freeMem(strInfoLog);
    end;
    for i:= 0 to high(shaderList) do
    glDetachShader(prog, shaderList[i]);
    result:=prog;
end;

end.

