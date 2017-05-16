{*************************************************************
*      Module: Ticket SpinLock with proportional backoff

*      Version: 1.01
*      Author: Amine Moulay Ramdane                
*     Company: Cyber-NT Communications           
*     
*       Email: aminer@videotron.ca   
*     Website: http://pages.videotron.com/aminer/
*        Date: September 12, 2013
*    Last update: September 23, 2013
*    
* Copyright © 2013 Amine Moulay Ramdane.All rights reserved
*
*************************************************************}

unit TicketSpinLock;

{$I defines.inc}

{$ifdef FPC}
{$mode delphi} 
{$endif}

{$IFDEF FPC}
{$ASMMODE intel}
{$ENDIF FPC}

interface

uses {$IF defined(Windows32) or  defined(Windows64) }
   Windows,
  {$IFEND}
sysutils,syncobjs;

const
  Alignment = 64; // alignment, needs to be power of 2

type

{$IFDEF CPU64}
int = int64;
Long = uint64;
{$ENDIF CPU64}
{$IFDEF CPU32}
int = integer;
Long = longword;
{$ENDIF CPU32}

typecache1  = array[0..14] of integer;
typecache2  = array[0..13] of integer;


MyRecord2 = Record  
  FCount2:long;
  {$IFDEF CPU32}
  cache:typecache1;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  cache:typecache2;
  {$ENDIF CPU64} 
end;

MyRecord3 = Record  
  FCount3:long;
  {$IFDEF CPU32}
  cache:typecache1;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  cache:typecache2;
  {$ENDIF CPU64} 
end;


PMyRecord2 = ^MyRecord2;
PMyRecord3 = ^MyRecord3;

TTicketSpinLock = class
  private
Buffer2: pointer;
FCount2: PMyRecord2;
Buffer3: pointer;
FCount3: PMyRecord3;


{$IF defined(Windows32) or  defined(Windows64) }
  FFileMapping2: THandle;
   FFileMapping3: THandle;

{$ELSE}
   

{$IFEND}
Fname:string;
mysleep:integer;
bool:boolean;
function GetUProcess: Boolean;
public
     constructor Create(const Name: string='';mssleep:integer=0);
    destructor  Destroy; override;

    procedure Enter; 
    procedure Leave; 
    
  end; { TMREW }

implementation

{$IF defined(Windows32) or  defined(Windows64) }
function SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';

//function GetCurrentProcessorNumber: longword; stdcall; external //kernel32 name 'GetCurrentProcessorNumber'; 

{$IFEND}


{$IF defined(CPU64) }
function LockedCompareExchange(CompareVal, NewVal: Int;  Target: pointer): Int; overload;
asm
mov rax, rcx
lock cmpxchg [r8], rdx
end;
{$IFEND}
{$IF defined(CPU32) }
function LockedCompareExchange(CompareVal, NewVal: int;  Target: pointer): int; overload;
asm
lock cmpxchg [ecx], edx
end;
{$IFEND}



function LockedCompareExchange1(var Target:int; Exch, Comp: Int): Int;assembler;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // EDX Exch
        // ECX Comp
        // <-- EAX Result
        XCHG EAX, ECX
        // EAX Comp
        // EDX Exch
        // ECX Target
        LOCK CMPXCHG [ECX], EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // EDX Exch
        // R8 Comp
        // <-- EAX Result
        MOV RAX, R8
        // RCX Target
        // EDX Exch
        // RAX Comp
        LOCK CMPXCHG [RCX], EDX
        {$ENDIF CPU64}
end;

function CAS(Target:pointer;Comp ,Exch : int): boolean;
var ret:int;
begin

//ret:=LockedCompareExchange(Target,Exch,Comp);
ret:=LockedCompareExchange(Comp,Exch,Target);
if ret=comp
 then result:=true
 else result:=false;  

end; { CAS }

function LockedIncLong(var Target: long): long;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, 1
        //sfence
       LOCK XADD [ECX], EAX
        inc     eax
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // <-- EAX Result
        MOV     rax, 1
        //sfence
        LOCK XADD [rcx], rax
        INC     rax
        {$ENDIF CPU64}
end;



constructor TTicketSpinLock.Create(const Name: string='';mssleep:integer=0);

var i:integer;
begin

FName := Name;

if (mssleep < 0) or (mssleep > 1) 
then 
raise Exception.Create('Error from the constructor: milliseconds sleep must be 0 or 1..');

mysleep:=mssleep;


{$IF defined(threads)}
    
    	   
Buffer2 := AllocMem(SizeOf(MyRecord2) + Alignment);
FCount2 := PMyRecord2((int(Buffer2) + Alignment - 1)
                           and not (Alignment - 1));  
Buffer3 := AllocMem(SizeOf(MyRecord3) + Alignment);
FCount3 := PMyRecord3((int(Buffer3) + Alignment - 1)
                           and not (Alignment - 1));  

{$ELSE}
if Name = '' then
  begin
    FFileMapping2 := 0;FFileMapping3 := 0;

    

Buffer2 := AllocMem(SizeOf(MyRecord2) + Alignment);
FCount2 := PMyRecord2((int(Buffer2) + Alignment - 1)
                           and not (Alignment - 1));  
Buffer3 := AllocMem(SizeOf(MyRecord3) + Alignment);
FCount3 := PMyRecord3((int(Buffer3) + Alignment - 1)
                           and not (Alignment - 1));  

 end
  else
  begin

FFileMapping2 := Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(MyRecord2)+ Alignment, PChar('TSpinLock_MMF2356_' + Name));
   Assert(FFileMapping2 <> 0);
    Buffer2 := Windows.MapViewOfFile(FFileMapping2, FILE_MAP_WRITE, 0, 0, 0);
    Assert(Buffer2 <> nil);
 
 FCount2 := PMyRecord2((int(Buffer2) + Alignment - 1)
                           and not (Alignment - 1));  


FFileMapping3 := Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(MyRecord3)+ Alignment, PChar('TSpinLock_MMF2356_' + Name));
   Assert(FFileMapping3 <> 0);
    Buffer3 := Windows.MapViewOfFile(FFileMapping3, FILE_MAP_WRITE, 0, 0, 0);
    Assert(Buffer3 <> nil);
 
 FCount3 := PMyRecord3((int(Buffer3) + Alignment - 1)
                           and not (Alignment - 1));  




  end;
{$IFEND}


end;

destructor TTicketSpinLock.Destroy;
begin

{$IF defined(threads)}
FreeMem(Buffer2);
FreeMem(Buffer3);

{$ELSE}
if GetUProcess then
   begin
    FreeMem(Buffer2);
    FreeMem(Buffer3);

  end
  else
  begin
    Windows.UnmapViewOfFile(Buffer2);
    Windows.CloseHandle(FFileMapping2);
    Windows.UnmapViewOfFile(Buffer3);
    Windows.CloseHandle(FFileMapping3);

  end;

{$IFEND}

  inherited Destroy;

end;

function TTicketSpinLock.GetUProcess: Boolean;
begin
{$IF defined(threads)}
    result:=true;
{$ELSE}
  Result := (FFileMapping2 = 0) and (FFileMapping3 = 0) ;
{$IFEND}

end;


//==============================================================================
procedure TTicketSpinLock.Enter;

var t,i,j:integer;
    bool:boolean;
    slot:long; 
begin

slot:=LockedIncLong(FCount2^.FCount2);

while ((slot-1) <> FCount3^.FCount3)
do
begin
j:=(slot-1)- FCount3^.FCount3;
for i:=0 to j*20 do asm  pause end;
//sleep(0);
end;


end;
//==============================================================================

procedure TTicketSpinLock.Leave;

begin
inc(FCount3^.FCount3);
sleep(0);
end;

end.