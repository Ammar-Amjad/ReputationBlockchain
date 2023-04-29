
% 1- rate of processing transactions OR average time taken for finding block in main blockchain.
% 2- rate of processing transactions OR average time taken for finding block in sidechains.
% 3- number of blocks found in main blockchain.
% 4- number of blocks found in each sidechain.
% 5- How many times the difficulty has been reduced.
% 6- How much time is saved for finding block in main blockchain due to difficulty reduction.

% main blockchain -> mining and reputation
%                 -> transfer
%                 -> mining and transfer
%                 -> clear reputation
-module(regionalblockchain).

-export([start/2, server/3, miner/4, add_block/1, create_block/2]).
-record(block, {index, timestamp, data, prev_hash, nonce}).

start(N, Diff) when N > 0 ->
    ServerPid = spawn(regionalblockchain, server, [0, 0, #{}]),
    spawn_miners(N, ServerPid, Diff).

spawn_miners(0, _, _) ->
    ok;
spawn_miners(N, ServerPid, Diff) when N > 0 ->
    spawn(regionalblockchain, miner, [ServerPid, Diff, regional, #{}]),
    spawn_miners(N - 1, ServerPid, Diff).

server(Count, TotalTime, RPIDMap) ->
    receive
        {miner, _, _, TimeTaken, RPID} ->
            NewCount = Count + 1,
            NewTotalTime = TotalTime + TimeTaken,
            AverageTime = NewTotalTime / NewCount,
            io:format("Average time taken to find block: ~p seconds~n", [AverageTime]),
            NewRPIDMap = update_rpid_map(RPIDMap, RPID),
            RPIDFingerTable = chord:get_ids(RPID),
            io:format("Map: ~p ~p ~n", [NewRPIDMap, RPIDFingerTable]),
            server(NewCount, NewTotalTime, NewRPIDMap)
    end.

update_rpid_map(RPIDMap, RPID) ->
    Reputation = 5,
    case maps:find(RPID, RPIDMap) of
        {ok, Count} ->
            NewRPIDMap = if Count + 1 >= Reputation ->
                            maps:put(RPID, 0, RPIDMap); % Set RPID value to 0 if it reaches 5
                         true ->
                            maps:put(RPID, Count + 1, RPIDMap)
                         end,
            NewRPIDMap;
        error -> maps:put(RPID, 1, RPIDMap)
    end.

miner(ServerPid, Diff, MiningType, RPIDMap) ->
    {Result, TimeTaken, PID} = pow:find_pow(Diff, MiningType),
    ServerPid ! {miner, self(), Result, TimeTaken, PID},
    NewRPIDMap = update_rpid_map(RPIDMap, PID),
    io:fwrite("~p ~n ~p ~p ~n", [Result, MiningType, maps:get(PID, NewRPIDMap)]),
    case MiningType of
        global ->
            NextDiff = case maps:get(PID, NewRPIDMap) of
                                5-1 -> Diff - 1;
                                _ -> Diff
                             end;
        regional ->
            NextDiff = case maps:get(PID, NewRPIDMap) of
                                5-1 -> Diff + 1;
                                _ -> Diff
                             end
    end,
    case MiningType of
        global ->
            NextMiningType = case maps:get(PID, NewRPIDMap) of
                                5-1 -> regional;
                                _ -> global
                             end;
        regional ->
            NextMiningType = case maps:get(PID, NewRPIDMap) of
                                5-1 -> rollup(NewRPIDMap),
                                    io:fwrite("Transactions rolled up~n"),
                                    global;
                                _ -> regional
                             end
    end,
    io:fwrite("Diff ~p ~n", [NextDiff]),
    miner(ServerPid, NextDiff, NextMiningType, NewRPIDMap).

    
create_block(PrevBlock, Data) ->
    Nonce = 0,
    PrevHash = PrevBlock#block.prev_hash,
    Index = PrevBlock#block.index + 1,
    Timestamp = os:system_time(millisecond),
    Block = #block{index = Index, timestamp = Timestamp, data = Data, prev_hash = PrevHash, nonce = Nonce},
    Block.

add_block(Blockchain) ->
    LastBlock = lists:last(Blockchain),
    validate_blockchain([Blockchain, LastBlock]),
    NewBlock = create_block(LastBlock, <<"Transaction">>),
    NewBlockchain = Blockchain ++ [NewBlock],
    NewBlockchain.

validate_blockchain([GenesisBlock | RestBlockchain]) ->
    validate_blockchain(RestBlockchain, GenesisBlock, true).

validate_blockchain([], _, Result) ->
    Result;
validate_blockchain([CurrentBlock | RestBlockchain], PrevBlock, Result) ->
    IndexCheck = CurrentBlock#block.index == PrevBlock#block.index + 1,
    PrevHashCheck = CurrentBlock#block.prev_hash == PrevBlock#block.prev_hash,
    NewResult = Result andalso IndexCheck andalso PrevHashCheck,
    IsValid = validate_blockchain(RestBlockchain, CurrentBlock, NewResult),
    io:format("Blockchain is valid: ~p~n", [IsValid]).

get_transactions_for_rpid(RPID) ->
    RPID.

summarize_transactions(Transactions) ->
    Transactions.

rollup(RPIDMap) ->
    RegionalBlocks = lists:filter(fun({_RPID, MiningType}) -> MiningType == regional end, maps:to_list(RPIDMap)),
    RollupTransactions = create_rollup_transactions(RegionalBlocks),
    RollupTransactions.

create_rollup_transactions(RegionalBlocks) ->
    lists:foldl(fun({RPID, _}, Acc) ->
                        Transactions = get_transactions_for_rpid(RPID),
                        SummarizedTransactions = summarize_transactions(Transactions),
                        [{RPID, SummarizedTransactions} | Acc]
                end, [], RegionalBlocks).

