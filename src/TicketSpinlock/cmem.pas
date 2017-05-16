unit cmem;

//{$mode objfpc}

interface

Function scalable_getmem(Size : Longint) : Pointer;cdecl;external 'tbbmalloc' name 'scalable_malloc';
Procedure scalable_freemem(P : Pointer); cdecl; external 'tbbmalloc' name 'scalable_free';
function scalable_realloc(P : Pointer; Size : longint) : pointer; cdecl;external 'tbbmalloc' name 'scalable_realloc';

implementation
{$IFDEF FreePascal}
Function CGetMem  (Size : Longword) : Pointer;
{$ENDIF}
{$IFDEF Delphi}
Function CGetMem  (Size : Longint) : Pointer;
{$ENDIF}
{$IFDEF Delphi2005+}
Function CGetMem  (Size : Longint) : Pointer;
{$ENDIF}

begin
  result:=scalable_getmem(Size);
end;

{$IFDEF FreePascal}
Function CFreeMem (P : pointer) : Longword;
{$ENDIF}
{$IFDEF Delphi}
Function CFreeMem (P : pointer) : Longint;
{$ENDIF}
{$IFDEF DELPHI2005+}
Function CFreeMem (P : pointer) : Longint;
{$ENDIF}


begin
  scalable_freemem(P);
  Result:=0;
end;

{$IFDEF FreePascal}
Function CReAllocMem (var p:pointer;Size:longword):Pointer;
{$ENDIF}
{$IFDEF Delphi}
Function CReAllocMem (p:pointer;Size:longint):Pointer;
{$ENDIF}
{$IFDEF DELPHI2005+}
Function CReAllocMem (p:pointer;Size:longint):Pointer;
{$ENDIF}


begin
  Result:=scalable_realloc(p,size);
end;


Const
 CMemoryManager : TMemoryManager =
    (
      GetMem : CGetmem;
      FreeMem : CFreeMem;
      //FreememSize : CFreememSize;
      //AllocMem : CAllocMem;
      ReallocMem : CReAllocMem;
      //MemSize : CMemSize;
      //MemAvail : CMemAvail;
      //MaxAvail : MaxAvail;
      //HeapSize : CHeapSize;
    );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);

Finalization
  SetMemoryManager (OldMemoryManager);
end.
