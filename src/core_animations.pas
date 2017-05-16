unit core_animations;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, core_types, VectorGeometry, core_listofrecords,
  fgl;

type

  { TActorAnimations }
  eHumanActorAnimations = (
                          asNone,
                          asStanding,
                          asWalking,
                          asRunning,
                          asCrouching,
                          asRollingStrafe,
                          asRollingForward,
                          asDying,
                          asClimbing,
                          asMax);

  sAnimationSet = set of eHumanActorAnimations;

  TAnimationList = specialize TFPGList <rAnim1f>;
  //holds together couple of animations that are launched at same time
  //for example to animate movement left and than forward

  { rAnimationSet }
  TCallBackProc = procedure of object;

  { TAnimationSet }

  TAnimationSet = class
   anims: array of rAnim1f;
   //nexta nimation to switch after end
   next:eHumanActorAnimations;
   //we can hook some event to end of animation set
   onEnd:TCallBackProc;
   typ:eHumanActorAnimations;
   procedure play;
   procedure stop;
   constructor create(leTyp:eHumanActorAnimations;len:integer);
   destructor destroy;override;
  end;
  { TActorAnimations }

  TActorAnimations = class
    fCurrentState:eHumanActorAnimations;
    //current animation
    anim:TAnimationSet;
    //those are definitions of postures. current animation will use those
    //array should be dynamic? not every kind of unit has every state
    anims:array [asStanding..asMax] of TAnimationSet;
    function setState(newState:eHumanActorAnimations):boolean;
    procedure play;//play current anim
    procedure stop;
    procedure update;
    constructor create;
    destructor destroy;override;
  end;

  //animations for general use
  TAnimationManager = class
   anims:TAnimationList;
  end;

  var
    //states that allow gravity/ground colision check
    animGravitables : sAnimationSet;
    animAllStates : sAnimationSet;
    //states that don't block user input
    animAbortables: sAnimationSet;
    //cos sie nie resetuja animacje przy zmianie

implementation

{ TAnimationSet }

constructor TAnimationSet.create(leTyp: eHumanActorAnimations; len: integer);
var i:integer;
begin
  setlength(anims,len);
  for i:=0 to len-1 do anims[i].typ:=integer(letyp);
  typ:=letyp;
end;

destructor TAnimationSet.destroy;
begin
 // setlength(anims,len);
  inherited destroy;
end;

procedure TAnimationSet.play;
var i:integer;
begin
  for i:=0 to length(anims)-1 do anims[i].play;
end;

procedure TAnimationSet.stop;
var i:integer;
begin
  for i:=0 to length(anims)-1 do anims[i].stop;

end;

{ TActorAnimations }

function TActorAnimations.setState(newState: eHumanActorAnimations): boolean;
begin
  result:=false;
  if newState=fCurrentState then exit;
  anim:=anims[newState];
  anim.play;
  fCurrentState:=newState;
  //log('set anim: '+EnumToStrIng(typeinfo(eHumanActorAnimations),integer(fCurrentState)));
  {
  case newState of
   //this one overrides everything else
    asDying:begin
      anim:=anims[asDying]
      anim.play;
    end;
    asStanding
  end;
  }
end;

procedure TActorAnimations.play;
begin
  anim.play;
end;

procedure TActorAnimations.stop;
begin
  anim.stop;
  //if anim.next<>asNone then setstate(anim.next);
  //if assigned(anim.onEnd) then anim.onEnd();
end;

procedure TActorAnimations.update;
var finished,i:integer;
    newValue:single;
begin
  finished:=0;
  for i:=0 to length(anim.anims)-1 do begin
  with anim.anims[i] do begin
    if playing=false then begin
       inc(finished);
       continue;
    end;
    if delay>0 then begin
       delay-=deltaT;
    end else if dither<>atWait then begin
     newValue:=value^;
     case dither of
      atLinear:newValue:=lerp(start,finish,time / duration);
      atPower:newValue:=InterpolatePower(start,finish,time / duration,10);
//      atQuad:
      //just a delay, no value change
      atSin:newValue:=InterpolateSin(start,finish,time / duration);
     end;
     time+=deltaT;
    //koniec?
     if time>=duration then begin
       inc(fcount);
       if loop=ltOneShot then repeats:=fcount //moze byc wtf
       else
       if loop=ltPingPong then begin //reverse
         if fcount mod 2 = 1 then begin
            start:=istop;
            finish:=istart;
         end else begin
            start:=istart;
            finish:=istop;
         end;
         time:=0;
         //if debugMsg<>'' then log('repeat: '+debugMsg);
       end;
       //send notification that anim ended?
       if (repeats>0) and (fcount=repeats) then begin//end
            playing:=false;
            time:=0;
            fcount:=0;
            delay:=idelay;
            if debugMsg<>'' then log(debugMsg);
       end;
     end;
     value^:=newValue;
    end;
  end;
  end;
  //check if all anims from set finished, if so then switch to next anim
  i:=length(anim.anims);
  if finished=i then begin
    if assigned(anim.onEnd) then anim.onEnd();
    if anim.next<>asNone then setState(anim.next) else begin
      fCurrentState:=asNone;
    end;
  end;
end;

constructor TActorAnimations.create;
begin
  fillchar(anims,sizeof(TAnimationSet),0);
  //for i:=asNone to asMax do anims[i].free;
end;

destructor TActorAnimations.destroy;
var i:eHumanActorAnimations;
begin
  for i:=low(anims) to high(anims) do if anims[i]<>nil then
      anims[i].free;
  inherited destroy;
end;


initialization

animAllStates:=[asStanding..asClimbing];
animGravitables:=animAllStates - [asClimbing];
animAbortables:=[asStanding,asWalking,asRunning]

end.

