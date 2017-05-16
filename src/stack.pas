unit stack;

interface

uses
  SysUtils;

type

  { TStack }

  TStack = class
  private
    //This hold items for our stack
    Items: array of variant;
    //Top of stack and stack count
    Tos, sCount: integer;
  public
    //Push items onto stack
    procedure Push(item: variant);
    //Pop items of stack
    function Pop(): variant;
    //Return stack count
    property Count: integer read sCount;
    //To create the stack
    constructor Create(n: integer);
  end;


implementation

{ TStack }

procedure TStack.Push(item: variant);
begin
  //Push item onto stack
  Items[tos] := item;
  //Dec tos
  Inc(tos);
end;

function TStack.Pop(): variant;
begin
  //Check if we hit bottom of stack and reset
  if tos = 0 then
    tos := Count;
  //Dec tos
  Dec(tos);
  //Return item form stack
  Result := Items[tos];
end;

constructor TStack.Create(n: integer);
begin
  Tos := 0;
  sCount := n;
  //Set size of stack
  SetLength(Items, sCount);
end;
end.
