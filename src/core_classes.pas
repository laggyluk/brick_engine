unit core_classes;

{$mode objfpc}{$H+}

interface
uses Classes,fgl,dglOpenGL;

type
  TStrGluintMap = specialize TFPGMap<string,GLuint>;
  TStrStringListMap = specialize TFPGMap<string,TStringList>;

  TUpdateable = class
    procedure update;virtual;abstract;
    procedure update(t:cardinal);virtual;abstract;
  end;
  TUpdateablesList = specialize TFPGList<TUpdateable>;

  { TVisualObject }
{
  TVisualObject = class (TUpdateable)
    visible:boolean; //marks for rendering
    center:TVector3f; //center of object (and hitbox)
    hitBoxSize:TAffineVector;
    function boundingBoxHitTest(rayStart,rayDir:TAffineVector;var crsPoint):boolean;virtual;
  end;
 }
implementation

{ TVisualObject }
{
function TVisualObject.boundingBoxHitTest(rayStart, rayDir: TAffineVector;
  var crsPoint): boolean;
begin
  result:= RayCastBoxIntersect(rayStart,rayDir,
           VectorSubtract(center, hitBoxSize),
           VectorAdd(center, hitBoxSize),@crsPoint);
end;
}
end.

