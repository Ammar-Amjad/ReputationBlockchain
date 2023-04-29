
-module(chord).
-export([lastNodetoCurrent/2, notify/2, updateFingerTables/1, createi/1, create/2,
  findNextNode/3, joinNodetoChord/2, stabilizeChord/1, runApp/2, reverse/1, updateAtIdx/3, get_ids/1]).

reverse(L) -> reverse(L,[]).
reverse([],R) -> R;
reverse([H|T],R) -> reverse(T,[H|R]).

updateAtIdx(L, Idx, Val) ->
  if (length(L) < Idx) -> L ++ [Val];
    true -> lists:sublist(L, Idx - 1) ++ [Val] ++ lists:nthtail(Idx, L)
  end.

makeChordApp(Nodes) -> makeChordApp(Nodes, 1).
makeChordApp(Nodes, Idx) ->
  if Idx == length(Nodes) ->
    { _, NA } = lists:nth(Idx, Nodes),
    { N2, NB } = lists:nth(1, Nodes),
    NA ! {'join', { N2, NB}, self()},
    receive {'joinNode'} -> io:format("- Chord Constructed Successfully!~n") end;
  true ->
    { _, NA } = lists:nth(Idx, Nodes),
    { N2, NB } = lists:nth(Idx + 1, Nodes),
    NA ! {'join', { N2, NB}, self()},
    receive  {'joinNode'} -> makeChordApp(Nodes, Idx + 1) end
  end.

createi(_) ->
  [<<$<, $0, $.,
  (erlang:integer_to_binary(rand:uniform(99)))/binary, $.,
  $0, $>>> || _ <- lists:seq(1, 4)].

startApp(Nodes, NumofRequests) ->
  makeChordApp(Nodes),
  stabilizeApp(Nodes),
  updateFingersTableApp(Nodes),
  avgHopsApp(Nodes, NumofRequests, 0, NumofRequests).

stabilizeApp(Nodes) ->
  lists:foreach(fun({ _, NID }) -> 
    NID ! {'stabilize', self()},
    receive {'updateStabilize'} -> continue end
    end, Nodes),
  io:format("- Chord Syncronized!~n").

updateFingersTableApp(Nodes) ->
  lists:foreach(fun({ _, NID }) -> 
    NID ! {'syncFingers', self()},
    receive {'returnSyncFingers'} -> continue end 
    end, Nodes),
  io:format("- Finger Table Syncronized!~n").

avgHopsApp(Nodes, NumofRequests, Count, I) ->
  if 
    (I == 0) -> io:format("-> Average hops: ~p~n", [((Count / 2) / NumofRequests)]);
    true-> 
      {_, PID} = lists:nth(rand:uniform(length(Nodes) - trunc(length(Nodes) / 2)), Nodes),
      PID ! {'nextNode', self(), rand:uniform(length(Nodes) - trunc(length(Nodes) / 2)), 0},
      receive {'getNextNode',{ _, HopCounter}} -> avgHopsApp(Nodes, NumofRequests, Count + HopCounter, I - 1) end
  end.

runApp(NumofNodes, NumofRequests) ->
  LogNodes = trunc(math:log2(NumofNodes)),
  [{ N - 1, spawn_link(chord, create, [LogNodes, N - 1, NumofRequests]) } || N <- lists:seq(1, NumofNodes)].

lastNodetoCurrent(_, {_, _, ID, _, [] }) -> {ID, self()}; 
lastNodetoCurrent(Key, Variables) ->
  { LogNodes, N, ID, PrevNode, [{ H, PID } | T] } = Variables,
  if (ID < H) and (H < Key) -> { H, PID };
    true -> lastNodetoCurrent(Key, { LogNodes, N, ID, PrevNode, T })
  end.
startChord(Variables) ->
  receive
    {'notify', Node} -> NewState = notify(Node, Variables), startChord(NewState);
    {'join', Node, PID} -> NewState = joinNodetoChord(Node, Variables), PID ! {'joinNode'}, startChord(NewState);
    {'syncFingers', PID} -> NewState = updateFingerTables(Variables), PID ! {'returnSyncFingers'}, startChord(NewState);
    {'predecessor', PID} -> { _, _, _, PrevNode, _ } = Variables, PID ! {'prevNode', PrevNode}, startChord(Variables);
    {'stabilize', PID} -> NewState = stabilizeChord(Variables), PID ! {'updateStabilize'}, startChord(NewState);
    {'nextNode', PID, Key, HopCounter} -> PID ! {'getNextNode', findNextNode(Key, Variables, HopCounter)}, startChord(Variables);
    {'prevNode', PID, Key} -> PID ! {'getPrevNode', lastNodetoCurrent(Key, Variables)}, startChord(Variables)
  end.

findNextNode(Key, { LogNodes, N, ID, PrevNode, FTableEntry }, HopCounter) ->
  [{ NextID, NextPID } | _] = FTableEntry,
  if (ID < Key) and (Key =< NextID) -> {{ NextID, NextPID }, HopCounter};
    true ->
      {CID, CPID} = lastNodetoCurrent(Key, { LogNodes, N, ID, PrevNode, reverse(FTableEntry) }),
      if CID == ID -> {{ NextID, NextPID }, HopCounter};
        true -> CPID ! {'nextNode', self(), Key, HopCounter + 1},
          receive {'getNextNode', {Next, AvgHopCount}} -> {Next, AvgHopCount} end
      end
  end.


joinNodetoChord({ PN, PID }, Variables) ->
  { LogNodes, N, ID, PrevNode, FTableEntry } = Variables,
  Key = ID rem trunc(math:pow(2, LogNodes)),
  PID ! {'nextNode', self(), Key, 0},
  receive
    {'getNextNode', {Next, _}} -> 
      { NextID, _ } = Next,
      if 
        NextID == ID -> { LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, 1, {PN, PID}) };
        true -> { LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, 1, Next) }
      end
  end.

create(LogNodes, N) ->
  startChord({LogNodes, N, N, none, [{ N, self() }]}). 

stabilizeChord({ LogNodes, N, ID, PrevNode, FTableEntry }) ->
  [{NextID, NextPID} | _] = FTableEntry,
  if (ID == NextID) -> { LogNodes, N, ID, PrevNode, FTableEntry };
    true -> 
      NextPID ! {'predecessor', self()},
      receive
        {'prevNode', X} ->
          if 
            X == none -> 
              NextPID ! {'notify', {ID, self()}},
              { LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, 1, {NextID, NextPID})};
            true ->
              {X_Identifier, X_PID} = X,
              if (ID < X_Identifier) and (X_Identifier < NextID) ->
                X_PID ! {'notify', {ID, self()}},
                { LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, 1, X)};
                true -> { LogNodes, N, ID, PrevNode, FTableEntry }
              end
          end
      end
  end.
  

notify({ NodeID, NID }, {LogNodes, N, ID, PrevNode, FTableEntry}) ->
  if PrevNode == none -> {LogNodes, N, ID, { NodeID, NID }, FTableEntry};
    true -> { PrevID, _ } = PrevNode,
      if (PrevID < NodeID) and (NodeID < ID) -> {LogNodes, N, ID, { NodeID, NID }, FTableEntry};
        true -> {LogNodes, N, ID, PrevNode, FTableEntry}
      end
  end.


updateFingerTables(Variables) ->
  { LogNodes, N, ID, PrevNode, FTableEntry } = Variables,
  Key = (ID + trunc(math:pow(2, 0))) rem trunc(math:pow(2, LogNodes)),
  { FN, _ } = findNextNode(Key, { LogNodes, N, ID, PrevNode, FTableEntry }, 0),
  updateFingerTables(2, { LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, 1, FN)}).

updateFingerTables(Next, Variables) -> 
  { LogNodes, N, ID, PrevNode, FTableEntry } = Variables,
  if Next > LogNodes -> { LogNodes, N, ID, PrevNode, FTableEntry};
    true ->
      Key = (ID + trunc(math:pow(2, Next - 1))) rem trunc(math:pow(2, LogNodes)),
      { FN, _ } = findNextNode(Key,  { LogNodes, N, ID, PrevNode, FTableEntry }, 0),
      updateFingerTables(Next + 1, {LogNodes, N, ID, PrevNode, updateAtIdx(FTableEntry, Next, FN)})
  end.

get_ids(NNodes) ->
  createi(NNodes).
