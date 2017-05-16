unit core_behaviour_tree;
{ based on Bjoern Knafla's article:
  http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, core_types, xmlread, dom, core_beh_tree_types;

type

//  TActionProcedure = function:nodeStates of object;
  TConditionFunction = function:boolean of object;

  TBehNodesList = class;
  TNodeOrder = (noFirst,noLast);
  { TBehTreeNode }

  TBehTreeNode = class
  private
    factorID: integer;
    procedure setActorID(AValue: integer);
    procedure setState(AValue: nodeStates);
   protected
    fstate:nodeStates;
    paramCount:integer;
    params:TFuncParams;
    function getState: nodeStates;
   public
    //used for remote debugging
    nodeID:integer;
    //last visited kid
    lastKid:integer;
    kids: TBehNodesList;
    parent:^TBehTreeNode;
    //just some info
    text:string;
    //if true then return success without evaluation
    skip:boolean;
    //parameters for specific node use. ie count of failed subnodes
    int1,int2:integer;
    //type of selectorNode
    typ:nodeTypes;
    luaFunc:string;
    luaParams:string;
    //pointers to assigned actor procedures
    action:TBehFuncType;//points to procedure that should be executed by action
    condition:TConditionFunction;
    //when actor learns new skill or sth, node for that skill should be added to the tree
    function AddTreeNode(const parentNodeName,filename:string;order:TNodeOrder):boolean;
    procedure update;
    property state:nodeStates read getState write setState;
      //same for every node in a tree, holds id of owning actor
    property actorID:integer read factorID write setActorID;
    procedure load(filename:string);
    function loadNodes(b:tbehtreenode;n:TDOMNode):tbehtreenode;
    procedure reset;
    constructor create(nodeTyp:nodeTypes;_actionCallback:TBehFuncType);
    destructor destroy;override;
  end;

   _TBehNodesList = specialize TFPGList<TBehTreeNode>;
   TBehNodesList = class(_TBehNodesList);

   procedure setRemoteBrain(actorID:integer);

 var
   //for remote brain debugging
   remoteBrainDebugMsg:TLogProcedure;
   remoteBrainDebugging:boolean = false;

implementation

var
  //used for remote debugging. if remoteBrain = node's actorID then send packets
  remoteBrain:integer = -1;

procedure setRemoteBrain(actorID: integer);
begin
  remoteBrain:=actorID;
end;

{ TBehaviourTree }

{ TBehTreeNode }

procedure TBehTreeNode.setActorID(AValue: integer);
var k:TBehTreeNode;
begin
  if factorID=AValue then Exit;
  factorID:=AValue;
  for k in kids do k.actorID:=factorID;
end;

procedure TBehTreeNode.setState(AValue: nodeStates);
begin
  fstate:=avalue;
  if (remoteBrainDebugging) and (remoteBrain = actorID) then remoteBrainDebugMsg(Format('%d%.*d', [avalue,4, nodeID]));
//  inttostr(nodeID)+inttostr(integer(avalue)))
end;

function TBehTreeNode.getState: nodeStates;
var
  i,failCount:integer;
  st:nodeStates;
  s:string;
  txt:pstring;
begin
  if skip then begin
     result:=nsSuccess;
     state:=nsSuccess;
     exit;
  end;
  result:=nsFailed;
  txt:=@text;
//each node type needs diferrent processing
  if typ = ntPriority then begin
    //if won't call last succesfull kid then funcStep in actor will get messed up
    //check all the kids. first success or running means this node succeded too
     if lastKid=-1 then lastKid:=0;
    for i:=lastKid to kids.Count-1 do begin
        st:=kids.Items[i].state;
        if (st=nsSuccess) or (st=nsRunning) then  begin
           state:=nsSuccess;
           result:=nsSuccess;
           lastKid:=i;
           //if last kid is successful then reset node
           if (i=kids.count-1) and (st=nsSuccess) then begin
             reset;
             log('priority resert');
           end;
           exit;
        end;
        //if here then no succesfull kid or no kids at all
        result:=nsFailed;
    end;
    //if here then none of kids succeded nor is running
    state:=nsFailed;
  end else

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
        if int1>=int2 then begin
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
  end else
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
  end else

  if typ = ntDecorator then begin
     state:=nsSuccess;
     result:=nsSuccess;
  end else

  if typ = ntCondition then begin
    //s:=text;
    //evaluate actors function and set result acordingly
    if luaFunc='' then log('ai tree node:' + text +' doesn''t have condition assigned!') else
    begin
//       st:=nodestates(logic.lua.callFunc(actorID,luafunc));  //and what about nsRunning?
       //st:=nodestates(logic.callActorFunc(actorID,luafunc,@params));
       st:=action(luafunc,@params,actorID,self);
    end;
    state:=st;
    result:=st;
    //if (parent<>nil) and (st=nsFailed) then parent.state:=nsFailed;
    exit;
  end;

  if (typ = ntNot) then if kids.Count>0 then begin
    st:=kids[0].state;
    result:=st;
    if st=nsSuccess then result:=nsFailed
    else if st=nsFailed then result:=nsSuccess;
    state:=result;
  end;

  if (typ = ntAction) or (typ=ntActionNot) then begin
    //s:=text;
    //if assigned(action) then log('ai tree node:' + text +' doesn''t have action assigned!') else
    if luaFunc='' then log('ai tree node:' + text +' doesn''t have action assigned!') else
    begin
      //st:=nodestates(logic.callActorFunc(actorID,luafunc,@params));
      st:=action(luafunc,@params,actorID,self);
    end;
    result:=st;
    if typ=ntActionNot then begin
      if result=nsSuccess then result:=nsFailed
      else if result=nsFailed then result:=nsSuccess;
    end;
    state:=st;
  end;
end;

function TBehTreeNode.AddTreeNode(const parentNodeName, filename: string;
  order: TNodeOrder): boolean;
var
   n,node,n2:TBehTreeNode;
begin
  result:=false;
  //find the parent node by name
  if text=parentNodeName then begin
    //first find the screen position of outmost right kid ? maybe not
    node:=TBehTreeNode.create(ntSequence,action);
    node.load(filename);
    node.text:=extractfilename(filename);
    //add- at end, insert at beginning?
    kids.Add(node);
    result:=true;
    end
  else for n in kids do if n.addtreeNode(parentNodeName,filename,order) then begin
    result:=true;
    break;
  end;
  //if result=false then
  //  log('TBehTreeNode::AddTreeNode() parentNode not found by name');
end;

procedure TBehTreeNode.update;
begin
  getstate;//will update all the kids
end;

function TBehTreeNode.loadNodes(b:tbehtreenode;n:TDOMNode):tbehtreenode;
var
  k:TDOMNode;
  i:integer;
  s:string;
  kid:tbehtreenode;
  list:tstringlist;
begin
  //read nodeID
  s:=n.nodeName;
  if length(s)=4 then i:=-1
  else i:=strtoint(copy(s,5,length(s)-4));
  b.nodeID:=i;
  s:=n.Attributes.GetNamedItem('name').NodeValue;
  b.text:=s;
  b.typ:=nodeTypes(strtoint(n.Attributes.GetNamedItem('type').NodeValue));
  b.luaFunc:=n.Attributes.GetNamedItem('func').NodeValue;
  b.luaparams:=n.Attributes.GetNamedItem('params').NodeValue;
  //read function parameters
  if b.luaParams<>'' then begin
    //break string to lines
    list:=tstringlist.Create;
    ExtractStrings([';'], [], PChar(b.luaParams), List);
    setlength(b.params,list.Count);
    b.paramCount:=list.count;
    for i:=0 to b.paramCount-1 do b.params[i]:=list[i];
    list.free;
  end;

 // b.notes:=n.GetAttributeNS('notes','');
 // b.group:=n.GetAttributeNS('sub','');
//  b.screenPos.x:=n.GetAttributeNS('x','');
//  b.screenPos.y:=n.GetAttributeNS('y','');
  b.skip:=strtobool(n.Attributes.GetNamedItem('skip').NodeValue);
  for i:=0 to n.ChildNodes.Count-1 do begin
      kid:=tbehtreenode.create(ntpriority,action);
      b.kids.Add(kid);
      loadNodes(kid,n.ChildNodes[i]);
  end;
end;

procedure TBehTreeNode.load(filename: string);
var
  Node: TDOMNode;
  Doc: TXMLDocument;
  j:integer;
begin
  try
    // Read in xml file from disk
    ReadXMLFile(Doc, filename);
    // Retrieve the "password" node
    Node := doc.DocumentElement.FirstChild;//Doc.DocumentElement.FindNode('Root');
    loadNodes(self,node);
  finally
    // finally, free the document
    Doc.Free;
  end;
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

constructor TBehTreeNode.create(nodeTyp: nodeTypes;
  _actionCallback: TBehFuncType);
begin
  typ:=nodetyp;
  lastKid:=-1;
  fstate:=nsReady;
  action:=_actionCallback;
  //action and condition doesn't need kids so dont create them
  if (nodeTyp<>ntAction) and(nodeTyp<>ntCondition) then kids:=TBehNodesList.create();
end;

destructor TBehTreeNode.destroy;
var
  i:integer;
begin
  if kids<>nil then begin
     for i:=0 to kids.Count-1 do kids[i].destroy;
     kids.Destroy;
  end;
  inherited;
end;

end.
