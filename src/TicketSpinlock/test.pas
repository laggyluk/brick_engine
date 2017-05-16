
program test;

{$I defines.inc}

uses 
{$IFDEF Unix}cthreads,{$ENDIF}
{$IF defined(Windows32) or  defined(Windows64) }
windows,
{$IFEND}
sysutils,fifoqueue_mpmc,classes,syncobjs;

 
type 

TStudent = class
 i:integer;    
 Name: string;
  end;

MyRecord = Record  
  name:string;
  number:integer;
 end;

TThread1 = class (TThread)
  private
     protected
    procedure Execute; override;
  public
   
  end;

TThread2 = class (TThread)
  private
     protected
    procedure Execute; override;
  public
   
  end;


var

thread1:TThread1;
thread2:TThread2;
i:integer;
t1:array of tthread;
a1,a2,n:longword;
Tqueue:Tfifoqueue_MPMC;
d:double;


procedure TThread1.execute;
var i:integer;
obj:^myrecord;
 begin 
for i:=0 to 999999

   
do 
begin

new(obj);
obj^.Name:='Amine';
obj^.number:=12;   
     
     if not Tqueue.push(tobject(obj))
           then 
            begin
              writeln('push overflow');
              exit;
            end;

end; 
end;

procedure TThread2.execute;

var i:integer;
temp:^myrecord;

begin
for i:=0 to 999999
do 
begin
while not tqueue.pop(tobject(temp)) do sleep(0); 
dispose(temp);
end;


end;

begin
//  Application.Title:='My Application';
tqueue:=TFIFOQUEUE_MPMC.create(25,true);

setlength(t1,8);
a1:=GetTickCount;
for i:=0 to 1
do t1[i]:=TThread1.create(false);
for i:=2 to 3
do t1[i]:=TThread2.create(false);

for I := 0 to 3 do t1[i].WaitFor;
a2:=GetTickCount;


writeln('Time in milliseconds is: ',a2-a1);

d:=(1000000.0*1000.0*4)/(a2-a1);

writeln('Queue length is: ',tqueue.length);
writeln('Throuput is: ' +Format('%.2f',[d])+' transations/s');





for i:=0 to 7 do t1[i].free;
tqueue.free;
setlength(t1,0);

end.


