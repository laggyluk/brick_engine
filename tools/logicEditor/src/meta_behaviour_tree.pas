unit meta_behaviour_tree;
{ based on Bjoern Knafla's article:
  http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
}

interface

uses
  Classes, SysUtils, system.Generics.collections, generics.defaults, system.types;

type

  nodeStates = (

    nsFailed,
    nsReady,//reqdy for query
     //
    nsRunning, //didn't finished evaluation in last tick
    nsSuccess
    );

  //types of branch nodes
  nodeTypes = (
     ntPriority, {On each traversal priority selectors check which child to run
                  in priority order until the first one succeeds or returns that
                  it is running. One option is to call the last still running
                  node again during the next behavior tree update. The other
                  option is to always restart traversal from the highest
                  priority child and implicitly cancel the last running child
                  behavior if it isn’t chosen immediately again.
                  int1>-1 is index of child that was running in last tick}

     ntSequence,{ run one child to finish after the other. If one or multiple fail
                  the whole sequence fails, too. Without a reset or without
                  finishing the last child node a sequence stores the last
                  running child to immediately return to it on the next update.}

     ntLoop,    {like sequences but they loop around (hah, who would have thought!)
                  when reaching their last child during their traversal instead of
                  returning to their parent node like sequence node do.}

     ntRandom,  { randomly (hah again) select which of their child nodes to visit.
                  A running node is visited again until it finishes.}

     ntConcurrent, {Concurrent nodes visit all of their children during each traversal.
                    A pre-specified number of children needs to fail to make
                    the concurrent node fail, too. Instead of running its child
                    nodes truly in parallel to each other there might be a
                    specific traversal order which can be exploited when adding
                    conditions (see below) to a concurrent node because an early
                    failing condition prevents its following concurrent siblings
                    from running.}

     ntDecorator, {decorator nodes typically have only one child and are used to enforce a
                   certain return state or to implement timers to restrict how often
                   the child will run in a given amount of time or how often it can
                   be executed without a pause.}

    //ain't selector but a leaf node

     ntAction, {which finally implement an actors or game world state changes,
                for example to plan a path and move on it, to sense for the nearest
                enemies, to show certain animations, switch weapons, or run a
                specified sound. Actions will typically coordinate and call into
                different game systems. They might run for one simulation tick –
                one frame – or might need to be ticked for multiple frames to
                finish their work.}

     ntCondition { check that certain actor or game world states hold true.
                    If a sequence node has a condition as one of its children
                    then the failing of the condition will prevent the following
                    nodes from being traversed during the update. When placed
                    below a concurrent node, conditions become a kind of invariant
                    check that prevents its sibling nodes from running if a
                    necessary state becomes invalid.}
      );
  //possible condition checks
  //nodeConditions = (

  TActionProcedure = function:nodeStates of object;
  TConditionFunction = function:boolean of object;

  { TBehTreeNode }

  TBehTreeNode = class
  private
    function getKidsCount: integer;
   protected
    //last kid index that was checked during update
    lastKid:integer;
    kids: TList<TBehTreeNode>;
    function getState: nodeStates;
   public
    //used for remote debugging
    nodeID:integer;
    fstate:nodeStates;
    parent:TBehTreeNode;
    text:string;
    notes:string;
    skip:boolean; //if true then return success without evaluation
    //parameters for specific node use. ie count of failed subnodes
    int1,int2:integer;
    //type of selectorNode
    typ:nodeTypes;
    //pointers to assigned actor procedures
    action:TActionProcedure;//points to procedure that should be executed by action
    condition:TConditionFunction;
    luaFunc:string;
    luaParams:string;
    //node position for visual editor
    screenPos:tpointf;
    group:boolean;//children folding flag for visualization
    procedure update;
    property state:nodeStates read getState write fstate;
    procedure addKid(b:tbehtreenode);
    procedure removeKid(k:tbehtreenode);
    procedure deleteKid(index:integer);
    property count:integer read getKidsCount;
    function getKid(index:integer):tbehtreenode;
    //save node & kids to file
    procedure saveToFile(filename:string);
    procedure reset;
    procedure sortByScreenPosition;
    procedure moveKidsScreeenPos(offset:tpointf);
    constructor create(nodeTyp:nodeTypes);
    destructor destroy;
  end;
  pBehTreeNode = ^TBehTreeNode;

 // procedure Sort(const AComparer: IComparer<TBehTreeNode>);

implementation
uses mega_editor_form;

var
  Comparison: TComparison<TBehTreeNode>;
   IntegerComparer: IComparer<Integer>;
{ TBehaviourTree }

{ TBehTreeNode }

function TBehTreeNode.getKid(index: integer): tbehtreenode;
begin
  result:=kids[index];
end;

function TBehTreeNode.getKidsCount: integer;
begin
  result:=kids.Count;
end;

function TBehTreeNode.getState: nodeStates;
var
  i,failCount:integer;
  st:nodeStates;
  s:string;
begin
  if skip then begin
     result:=nsSuccess;
     state:=nsSuccess;
     exit
  end;
  result:=nsFailed;
//each node type needs diferrent processing
  if typ = ntPriority then begin
    //check all the kids. first success or running means this node succeded too
    for i:=0 to kids.Count-1 do begin
        st:=kids.Items[i].state;
        if (st=nsSuccess) or (st=nsRunning) then  begin
           state:=nsSuccess;
           result:=nsSuccess;
           exit;
        end;
    end;
    //if here then none of kids succeded nor is running
    state:=nsFailed;
  end;

  if (typ = ntSequence) or (typ = ntLoop) then begin
    if fstate=nsFailed then begin
       state:=nsFailed;
       result:=nsFailed;
       exit;
    end;
    if fstate=nsSuccess then begin
       state:=nsSuccess;
       result:=nsSuccess;
       exit;
    end;
    if kids.Count=0 then begin //no kids. node is sort of invalid
        state:=nsFailed;
        result:=nsFailed;
        exit;
    end;
    int2:=0;//use as flag locally
    //check fail count
    //int1 will be a fail counter and int2 local flag
    if length(luafunc)=0 then int2:=0 else int2:=strtoint(luafunc);
    if int1>0 then if int1>=int2 then begin
       state:=nsFailed;
       result:=nsFailed;
       exit;
    end;
    //continue running last kid
    //fail if fail count is? 1
    if lastKid=-1 then inc(lastKid);
    if lastKid=kids.Count then begin
       if typ = ntLoop then begin
          reset;
          state:=nsRunning;
          result:=nsRunning;
//          lastKid:=-1;
//          int1:=0;
//          int2:=0;
        end else begin
        //reset node?
          state:=nsSuccess;
          result:=nsSuccess;
          lastKid:=-1;
        end;
        exit;
    end;
    st:=kids.Items[lastKid].state;
    if (st=nsSuccess) then begin
        state:=nsRunning;
        result:=nsRunning;
        lastKid:=lastKid+1;
        exit;
    end;
    if (st=nsRunning) then begin
        state:=nsRunning;
        result:=nsRunning;
        exit;
    end;
    if (st=nsFailed) then begin //if something is in luafunc use it as fail limit
         //condition child fails sequence?
        if kids.Items[lastKid].typ=ntCondition then begin
           state:=nsFailed;
           result:=nsFailed;
           exit;
        end;
        lastKid:=lastKid+1;
        inc(int1);
        state:=nsRunning;
        result:=nsRunning;
        exit;
    end;
  end;
    //state:=nsFailed;

  if typ = ntConcurrent then begin
    //run through all children untill certain number of fails is reached.
    //if less then node succeed
    failCount:=0;
    for i:=0 to kids.Count-1 do if kids.Items[i].state=nsFailed then begin
        inc(failCount);
        if failCount>=int1 then begin //number of fails reached. lets break
           state:=nsFailed;
           result:=nsFailed;
           exit;
        end;
        //on other hand if number of fails can't be reached then let's also break early
        if kids.Count-i<int1 then break;
    end;
    state:=nsSuccess;
    result:=nsSuccess;
    end;

  if typ = ntDecorator then begin
     state:=nsSuccess;
     result:=nsSuccess;
  end;

  if typ = ntCondition then begin
    //s:=text;
    //evaluate actors function and set result acordingly
    if luaFunc='' then log('ai tree node:' + text +' doesn''t have condition assigned!') else
    begin
       st:=nodestates(lua.callFunction(luafunc,luaparams));  //and what about nsRunning?
    end;
    state:=st;
    result:=st;
    //if (parent<>nil) and (st=nsFailed) then parent.state:=nsFailed;
    exit;
  end;

  if typ = ntAction then begin
    //s:=text;
    //if assigned(action) then log('ai tree node:' + text +' doesn''t have action assigned!') else
    if luaFunc='' then log('ai tree node:' + text +' doesn''t have action assigned!') else
    begin
      //st:=action();
      st:=nodestates(lua.callFunction(luafunc,luaparams));
    end;
    state:=st;
    result:=st;
  end;

end;

procedure TBehTreeNode.moveKidsScreeenPos(offset: tpointf);
var i:integer;
begin
  screenpos:=screenPos+offset;
  for i:=0 to kids.Count-1 do kids[i].screenPos:=kids[i].screenPos+offset;
end;

procedure TBehTreeNode.removeKid(k:tbehtreenode);
begin
  //k.destroy;
  kids.Remove(k);
end;

procedure TBehTreeNode.reset;
var i:integer;
begin
  fstate:=nsReady;
  //probably should be 'cased'
  int1:=0;
  int2:=0;
  lastKid:=-1;
  for i:=0 to kids.Count-1 do kids[i].reset;
end;

procedure toLines(n:tBehTreeNode;f:tstringlist);
var i:integer;
begin
  f.Add(n.text);
  f.Add(inttostr(integer(n.typ)));
  f.Add('');
  for i:=0 to n.kids.Count-1 do toLines(n.kids[i],f);
end;

procedure TBehTreeNode.saveToFile(filename: string);
var
  f:tstringlist;
begin
  try
    f:=tstringlist.Create;
    toLines(self,f);
    f.SaveToFile(filename);
  finally
    f.Free;
  end;
end;


procedure TBehTreeNode.sortByScreenPosition;
var i:integer;
begin
//  for i:=0 to kids[i] do if kids[i].
//         Result := round(Left.screenPos.x-Right.screenpos.x);
    Comparison :=  function(const Left, Right: TBehTreeNode): Integer
    begin
        Result := IntegerComparer.Compare(round(Left.screenPos.x),round(Right.screenPos.x));
    end;
    kids.Sort(TComparer<TBehTreeNode>.Construct(Comparison));
    for i:=0 to kids.Count-1 do kids[i].sortByScreenPosition;
end;

procedure TBehTreeNode.update;
begin
  getstate;//will update all the kids
end;

procedure TBehTreeNode.addKid(b:tbehtreenode);
begin
  b.parent:=self;
  kids.Add(b);
end;

constructor TBehTreeNode.create(nodeTyp: nodeTypes);
begin
  typ:=nodetyp;
  //action and condition doesn't need kids so dont create them
  kids:=TList<TBehTreeNode>.create;
  lastKid:=-1;
  fstate:=nsReady;
end;

procedure TBehTreeNode.deleteKid(index: integer);
begin
  kids[index].destroy;
  kids.delete(index);
end;

destructor TBehTreeNode.destroy;
var
  i:integer;
begin
  if kids<>nil then begin
     for i:=0 to kids.Count-1 do kids[i].destroy;
     kids.Destroy;
  end;
end;

initialization
  IntegerComparer := TComparer<Integer>.Default;

end.
