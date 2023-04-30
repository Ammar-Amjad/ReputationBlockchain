
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
-module(globalblockchain).

-export([start/3, server/4, miner/5, add_block/1, create_block/2]).
-record(block, {index, timestamp, data, prev_hash, nonce}).

start(N, Diff, Reputation) when N > 0 ->
    ServerPid = spawn(globalblockchain, server, [0, 0, #{}, Reputation]),
    spawn_miners(N, ServerPid, Diff, Reputation).

spawn_miners(0, _, _, _) ->
    ok;
spawn_miners(N, ServerPid, Diff, Reputation) when N > 0 ->
    spawn(globalblockchain, miner, [ServerPid, Diff, regional, #{}, Reputation]),
    spawn_miners(N - 1, ServerPid, Diff, Reputation).

server(Count, TotalTime, RPIDMap, Reputation) ->
    receive
        {miner, _, _, TimeTaken, RPID} ->
            NewCount = Count + 1,
            NewTotalTime = TotalTime + TimeTaken,
            AverageTime = NewTotalTime / NewCount,
            io:format("Average time taken to find block: ~p seconds~n", [AverageTime]),
            NewRPIDMap = update_rpid_map(RPIDMap, RPID, Reputation),
            RPIDFingerTable = chord:get_ids(RPID),
            io:format("Map: ~p ~p ~n", [NewRPIDMap, RPIDFingerTable]),
            server(NewCount, NewTotalTime, NewRPIDMap, Reputation)
    end.

update_rpid_map(RPIDMap, RPID, Reputation) ->
    case maps:find(RPID, RPIDMap) of
        {ok, Count} ->
            NewRPIDMap = if Count + 1 >= Reputation ->
                            maps:put(RPID, 0, RPIDMap);
                         true ->
                            maps:put(RPID, Count + 1, RPIDMap)
                         end,
            NewRPIDMap;
        error -> maps:put(RPID, 1, RPIDMap)
    end.

miner(ServerPid, Diff, MiningType, RPIDMap, Reputation) ->
    {Result, TimeTaken, PID} = pow:find_pow(Diff, MiningType),
    ServerPid ! {miner, self(), Result, TimeTaken, PID},
    NewRPIDMap = update_rpid_map(RPIDMap, PID, Reputation),
    io:fwrite("~p ~n ~p ~n", [Result, MiningType]),
    RPIDCount = maps:get(PID, NewRPIDMap),
    Rep = Reputation - 1,
    case MiningType of
        global ->
            NextDiff = case RPIDCount of
                                Rep -> Diff - 1;
                                _ -> Diff
                             end;
        regional ->
            NextDiff = case RPIDCount of
                                Rep -> Diff + 1;
                                _ -> Diff
                             end
    end,
    case MiningType of
        global ->
            NextMiningType = case RPIDCount of
                                Rep -> regional;
                                _ -> global
                             end;
        regional ->
            NextMiningType = case RPIDCount of
                                Rep -> rollup(NewRPIDMap),
                                    io:fwrite("Transactions rolled up~n"),
                                    global;
                                _ -> regional
                             end
    end,
    io:fwrite("Diff ~p ~n", [NextDiff]),
    miner(ServerPid, NextDiff, NextMiningType, NewRPIDMap, Reputation).


    
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




% 1- average time taken for finding block in main blockchain. - Done
% 3- number of blocks found in main blockchain in 30 sec with diff = 5. 22.5 - Done
