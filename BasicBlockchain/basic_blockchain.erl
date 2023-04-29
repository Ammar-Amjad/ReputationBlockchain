
% 1- rate of processing transactions OR average time taken for finding block in main blockchain.
% 2- rate of processing transactions OR average time taken for finding block in sidechains.
% 3- number of blocks found in main blockchain.
% 4- number of blocks found in each sidechain.
% 5- How many times the difficulty has been reduced.
% 6- How much time is saved for finding block in main blockchain due to difficulty reduction.

-module(basic_blockchain).
-export([start/2, server/2, miner/2]).

start(N, Diff) when N > 0 ->
    ServerPid = spawn(basic_blockchain, server, [0, 0]),
    spawn_miners(N, ServerPid, Diff).

spawn_miners(0, _, _) ->
    ok;
spawn_miners(N, ServerPid, Diff) when N > 0 ->
    spawn(basic_blockchain, miner, [ServerPid, Diff]),
    spawn_miners(N - 1, ServerPid, Diff).

server(Count, TotalTime) ->
    receive
        {miner, _, _, TimeTaken} ->
            % io:format("Miner ~p has reported the result: ~p~n", [MinerPid, Result]),
            NewCount = Count + 1,
            NewTotalTime = TotalTime + TimeTaken,
            AverageTime = NewTotalTime / NewCount,
            io:format("Average time taken to find block: ~p seconds~n", [AverageTime]),
            server(NewCount, NewTotalTime)
    end.

miner(ServerPid, Diff) ->
    {Result, TimeTaken} = pow:find_pow(Diff),
    ServerPid ! {miner, self(), Result, TimeTaken},
    miner(ServerPid, Diff).


% 1- average time taken for finding block in main blockchain. - Done
% 3- number of blocks found in main blockchain in 30 sec with diff = 5. 22.5 - Done
