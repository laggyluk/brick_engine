//http://codeflow.org/entries/2010/dec/09/minecraft-like-rendering-experiments-in-opengl-4/#volume-data
unit core_noise;

{$mode objfpc}{$H+}
{$ASMMODE intel}

interface

uses
  math, graphics, core_types;

  function simplex_noise(octaves:integer;x,y,z:single):single;
  function noise3d(xin,yin,zin:single):single;
  function PerlinNoise_2D(x,y:double):double;
  procedure plasmaClouds(width,height:integer;bmpBuffer:tbitmap);

implementation

const
  grad:array[0..11,0..2] of single = (
      (1.0,1.0,0.0),(-1.0,1.0,0.0),(1.0,-1.0,0.0),(-1.0,-1.0,0.0),
      (1.0,0.0,1.0),(-1.0,0.0,1.0),(1.0,0.0,-1.0),(-1.0,0.0,-1.0),
      (0.0,1.0,1.0),(0.0,-1.0,1.0),(0.0,1.0,-1.0),(0.0,-1.0,-1.0)
  );

  perm:array [0..511] of integer = (151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180, 151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180);


function dot(x,y,z:single; g:psingle):single;
begin
    result:= x*g[0] + y*g[1] + z*g[2];
end;

function noise3d(xin,yin,zin:single):single;
var
    F3, G3, t, X0, Y0, Z0, xx0, yy0, zz0, s, x1, y1, z1, x2, y2, z2, x3, y3, z3, t0, t1, t2, t3, n0, n1, n2, n3:single;
    i, j, k, ii, jj, kk, i1, j1, k1, i2, j2, k2, gi0, gi1, gi2, gi3:integer;
begin
    F3 := 1.0 / 3.0;
    s := (xin+yin+zin)*F3;
    i := round(xin+s);
    j := round(yin+s);
    k := round(zin+s);
    G3 := 1.0/6.0;
    t := (i+j+k)*G3;
    X0 := i-t;
    Y0 := j-t;
    Z0 := k-t;
    xx0 := xin-X0;
    yy0 := yin-Y0;
    zz0 := zin-Z0;

    if(xx0 >= yy0) then begin
        if(yy0 >= zz0) then begin
            i1:=1; j1:=0; k1:=0; i2:=1; j2:=1; k2:=0;
        end
        else if(xx0 >= zz0) then begin
             i1:=1; j1:=0; k1:=0; i2:=1; j2:=0; k2:=1;
        end
        else begin
            i1:=0; j1:=0; k1:=1; i2:=1; j2:=0; k2:=1;
        end
    end
    else begin
        if(yy0 < zz0) then begin
            i1:=0; j1:=0; k1:=1; i2:=0; j2:=1; k2:=1;
        end
        else if(xx0 < zz0) then begin
            i1:=0; j1:=1; k1:=0; i2:=0; j2:=1; k2:=1;
        end
        else begin
            i1:=0; j1:=1; k1:=0; i2:=1; j2:=1; k2:=0;
        end
    end;

    x1 := xx0 - i1 + G3;
//    end;
    y1 := yy0 - j1 + G3;
    z1 := zz0 - k1 + G3;
    x2 := xx0 - i2 + 2.0*G3;
    y2 := yy0 - j2 + 2.0*G3;
    z2 := zz0 - k2 + 2.0*G3;
    x3 := xx0 - 1.0 + 3.0*G3;
    y3 := yy0 - 1.0 + 3.0*G3;
    z3 := zz0 - 1.0 + 3.0*G3;

    ii := i and 255;
    jj := j and 255;
    kk := k and 255;

    gi0 := perm[ii+perm[jj+perm[kk]]] mod 12;
    gi1 := perm[ii+i1+perm[jj+j1+perm[kk+k1]]] mod 12;
    gi2 := perm[ii+i2+perm[jj+j2+perm[kk+k2]]] mod 12;
    gi3 := perm[ii+1+perm[jj+1+perm[kk+1]]] mod 12;

    t0 := 0.6 - xx0*xx0 - yy0*yy0 - zz0*zz0;
    if(t0<0) then begin
         n0 := 0.0;
    end
    else begin
        t0 := t0*t0;
        n0 := t0 * t0 * dot(xx0, yy0, zz0, grad[gi0]);
    end;

    t1 := 0.6 - x1*x1 - y1*y1 - z1*z1;
    if(t1<0) then begin
         n1 := 0.0;
    end
    else begin
        t1 := t1*t1;
        n1 := t1 * t1 * dot(x1, y1, z1, grad[gi1]);
    end;
    t2 := 0.6 - x2*x2 - y2*y2 - z2*z2;
    if(t2<0) then begin
         n2 := 0.0;
    end
    else begin
        t2 := t2*t2;
        n2 := t2 * t2 * dot(x2, y2, z2, grad[gi2]);
    end;
    t3 := 0.6 - x3*x3 - y3*y3 - z3*z3;
    if(t3<0) then begin
         n3 := 0.0;
    end
    else begin
        t3 := t3*t3;
        n3 := t3 * t3 * dot(x3, y3, z3, grad[gi3]);
    end ;
    result:= 16.0*(n0 + n1 + n2 + n3)+1.0;
end;

function simplex_noise(octaves:integer;x,y,z:single):single;
var
    value:single = 0.0;
    i:integer;
begin
    for i:=0 to octaves-1 do begin
        value := value + noise3d(
        x*power(2, i),
        y*power(2, i),
         z*power(2, i)
        );
    end;
    result:= value;
end;

//some other crap
procedure plasmaClouds(width,height:integer;bmpBuffer:tbitmap);
var plasma : array of array of byte;
         procedure halfway(const x1,y1,x2,y2: integer);

              procedure adjust(const xa,ya,x,y,xb,yb: integer);stdcall;
               var
                 v,v1,v2,t1: integer;
              begin
               if plasma[x,y]<>0 then exit;
               t1:=trunc((random-0.5)*(Abs(xa-xb)+Abs(ya-yb)));
               v1:=plasma[xa,ya];
               v2:=plasma[xb,yb];
                 asm
                 mov eax,t1
                 mov ecx,2
                 mul ecx
                 mul ecx
                 mov t1,eax
                 mov eax,v1
                 add eax,v2
                 shr eax,1
                 add eax,t1
                 mov v,eax
                 end;
               if v<1 then v:=1;
               if v>=193 then v:=192;
               plasma[x,y]:=v;
              end;

         var
            x,y: integer;
            v: double;
         begin
          if (x2-x1<2) and (y2-y1<2) then exit;
          asm
             //x:=(x1+x2) div 2;
             mov eax,x1
             add eax,x2
             shr eax,1
             mov x,eax
             //y:=(y1+y2) div 2;
             mov eax,y1
             add eax,y2
             shr eax,1
             mov y,eax
          end;
          adjust(x1,y1,x,y1,x2,y1);
          adjust(x2,y1,x2,y,x2,y2);
          adjust(x1,y2,x,y2,x2,y2);
          adjust(x1,y1,x1,y,x1,y2);
          if plasma[x,y]=0 then
             plasma[x,y]:=(plasma[x1,y1]+plasma[x2,y1]+plasma[x2,y2]+plasma[x1,y2]) div 4;
          halfway(x1,y1,x,y);
          halfway(x,y1,x2,y);
          halfway(x,y,x2,y2);
          halfway(x1,y,x,y2);
        end;
 var
   x,y: integer;
begin
  setlength(plasma,width,height);
//for x:=0 to width-1 do
//    for y:=0 to height-1 do
//        plasma[x,y]:=0;

  plasma[0,height-1]:=random(192);
  plasma[width-1,height-1]:=random(192);
  plasma[width-1,0]:=random(192);
  plasma[0,0]:=random(192);

  halfway(0,0,width-1,height-1);

  if bmpBuffer=nil then bmpBuffer:=tbitmap.create;
  bmpBuffer.width :=width;
  bmpBuffer.height:=height;

  for x:=0 to width-1 do
    for y:=0 to height-1 do
        bmpBuffer.canvas.pixels[x,y]:=RGBToColor(plasma[x,y],plasma[x,y],plasma[x,y]);
  bmpBuffer.SaveToFile(appPath+'terrain\test.bmp');
  //bmpBuffer.free; buffer is freed upon request
end;

function PerlinNoise_2D(x,y:double):double;
var i:integer;
         function IntNoise_2D(const x,y:integer):double;stdcall;
         var n:integer;
         begin
         asm
            mov eax,x
            mov ecx,57
            mul ecx
            add eax,y
            mov ecx,eax
            shl eax,13
            xor eax,ecx
            mul eax
            mov edx,109537
            mul edx
            add eax,107873
            mul eax
            add eax,1376312589
            and eax,$7fffffff
            mov n,eax
         end;
         IntNoise_2D:=1.0 - n / 1073741824.0;
         end;

         function SmoothNoise_2D(const x,y:integer):double;
         begin
         SmoothNoise_2D:=
               (( intnoise_2d(x-1, y-1)+intnoise_2d(x+1, y-1)
               +intnoise_2d(x-1, y+1)+intnoise_2d(x+1, y+1) ) / 16)+
               (( intnoise_2d(x-1, y)  +intnoise_2d(x+1, y)
               +intnoise_2d(x, y-1)  +intnoise_2d(x, y+1) ) /  8)+
               intnoise_2d(x, y) / 4;
         end;

         function InterpolatedNoise_2D(const x,y:double):double;
         var integer_x,integer_y:integer;
             v1,v2,v3,v4,i1,i2,fractional_x,fractional_y,f:double;
         begin
               integer_X    := trunc(x);
               fractional_X := x - integer_X;
               integer_y    := trunc(y);
               fractional_y := y - integer_y;

               v1 := SmoothNoise_2D(integer_X,integer_y);
               v2 := SmoothNoise_2D(integer_X + 1,integer_y);
               v3 := SmoothNoise_2D(integer_X,integer_y+1);
               v4 := SmoothNoise_2D(integer_X + 1,integer_y+1);
               f := (1 - cos(fractional_x * pi)) / 2;
               i1:=v1*(1-f)+v2*f;
               i2:=v3*(1-f)+v4*f;
               f := (1 - cos(fractional_y * pi)) / 2;
               InterpolatedNoise_2D:=i1*(1-f)+i2*f;
         end;

var n:integer;
    p,frequency,amplitude,total:double;
begin
   total := 0;
   p := 0.5; //persistance
   n := 3;//Octaves - 1;
   for i:=0 to n do begin
     frequency := power(2,i);
     amplitude := power(p,i);
     total := total + InterpolatedNoise_2D(x*frequency,y*frequency)*amplitude;
   end;
   PerlinNoise_2D:=total
end;

end.

