unit core_sphere;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,VectorGeometry,dglOpenGL;


implementation

type
   tvdata = array [0..11,0..2] of glfloat;
   Ttindices= array[0..19,0..2] of GLuint;

   rBuffer = packed record
     x,y,z:single; //vertex
     //,r,g,b,a:single;
     nx,ny,nz:single; //normal
   end;

   TSphereVerts = array of rBuffer;

const
   X  = 0.525731112119133606;
   Z  = 0.850650808352039932;

var
 index:integer;//position in sphere buf
 sphereVerts:TSphereVerts;

 vdata : tvdata = (
      (-X, 0.0, Z), (X, 0.0, Z), (-X, 0.0, -Z), (X, 0.0, -Z),
      (0.0, Z, X), (0.0, Z, -X), (0.0, -Z, X), (0.0, -Z, -X),
      (Z, X, 0.0), (-Z, X, 0.0), (Z, -X, 0.0), (-Z, -X, 0.0)
    );


   tindices: ttindices = (
    (0,4,1), (0,9,4), (9,5,4), (4,5,8), (4,8,1),
    (8,10,1), (8,3,10), (5,3,8), (5,2,3), (2,7,3),
    (7,10,3), (7,6,10), (7,11,6), (11,0,6), (0,1,6),
    (6,1,10), (9,0,11), (9,11,2), (9,2,5), (7,2,11) );



procedure normalize(a:TAffineVector);
var
   d:GLfloat;
begin
    d:=sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2]);
    a[0]/=d; a[1]/=d; a[2]/=d;
end;

procedure fillBuf(norm,vec:TAffineVector);
begin
  with sphereVerts[index] do begin
       nx:=norm[0];
       ny:=norm[1];
       nz:=norm[2];
       x:=vec[0];
       y:=vec[1];
       z:=vec[2];
  end;
  inc(index);
end;

procedure drawtri(a,b,c:PGLfloat; divs:integer;r:single);
begin
    if (divs<=0) then begin
        fillBuf(vector3fmake(
        glNormal3fv(a);
        glVertex3f(a[0]*r, a[1]*r, a[2]*r);
        glNormal3fv(b); glVertex3f(b[0]*r, b[1]*r, b[2]*r);
        glNormal3fv(c); glVertex3f(c[0]*r, c[1]*r, c[2]*r);
    end
    else begin
        GLfloat ab[3], ac[3], bc[3];
        for (int i=0;i<3;i++) {
            ab[i]=(a[i]+b[i])/2;
            ac[i]=(a[i]+c[i])/2;
            bc[i]=(b[i]+c[i])/2;
        }
        normalize(ab); normalize(ac); normalize(bc);
        drawtri(a, ab, ac, divs-1, r);
        drawtri(b, bc, ab, divs-1, r);
        drawtri(c, ac, bc, divs-1, r);
        drawtri(ab, bc, ac, divs-1, r);  //<--Comment this line and sphere looks really cool!
    end;
end;

procedure generateSphere(ndiv:integer; radius:single=1.0);
begin
//  glBegin(GL_TRIANGLES);
   index:=0;
   setlength(sphereVerts,1024);
   for (int i=0;i<20;i++)
        drawtri(vdata[tindices[i][0]], vdata[tindices[i][1]], vdata[tindices[i][2]], ndiv, radius);
//  glEnd();
end;

end.

