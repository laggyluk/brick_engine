unit core_beh_tree_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

     ntCondition, { check that certain actor or game world states hold true.
                    If a sequence node has a condition as one of its children
                    then the failing of the condition will prevent the following
                    nodes from being traversed during the update. When placed
                    below a concurrent node, conditions become a kind of invariant
                    check that prevents its sibling nodes from running if a
                    necessary state becomes invalid.}

     ntActionNot, {same as action but inverts result if success or failed}
     ntNot //similar to ActionNot but inverts single children result
      );
  //possible condition checks
  //nodeConditions = (

  TFuncParams = array of string;
  pFuncParams = ^TFuncParams;
  //node is using this function template to execute Action
  TBehFuncType = function(MethodName: string; const params:pFuncParams;actorID:integer;behNode:pointer): nodeStates of object;

implementation


end.

