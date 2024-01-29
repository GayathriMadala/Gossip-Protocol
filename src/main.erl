-module(main).
-export([get_group_name/2,get_random_node/5,get_random_node_line/3,
line_topology/5,push_sum_node/12,line_topology_push_sum/5, start/0 , full_topology/5, full_topology_push_sum/5]).
-export([twoD_topology/2]).
-export([twoD_topology_ps/2]).
-export([kill_2d_topology_nodes/3]).
-define(MaxRumour, 10).
-import(topologies, [get_2d_grid/2]).

get_group_name(Algorithm, Topology) ->
    string:concat(Algorithm, Topology).

get_random_node(_, Pid, processIdInGroup, PidMap, Topology) when is_map(PidMap) ->
    %AllNodes = pg:get_local_members(Group),
    if Topology == line ->
        AllNodes = get_random_node_line(Topology, Pid, PidMap),
        Value = lists:nth(rand:uniform(length(AllNodes)), AllNodes), 
        maps:get(Value, PidMap);
    Topology == full ->
        AllNodes = get_random_node_full(Topology, Pid,maps:remove(Pid,PidMap)), 
        Value = lists:nth(rand:uniform(length(AllNodes)), AllNodes),
        maps:get(Value, PidMap);
    Topology == "2D" ->
        AllNodes = get_random_node_2D(Topology, Pid, PidMap),
        Value = lists:nth(rand:uniform(length(AllNodes)), AllNodes), 
        maps:get(Value, PidMap);
    true ->
        ok
    end.

get_random_node_2D(_, Pid, PidMap) ->
    topologies:get_2d_neighbors(Pid, trunc(math:sqrt(maps:size(PidMap)))).

get_random_node_line(_, Pid, PidMap) ->
    MSize = maps:size(PidMap),
    if Pid == 1 ->
        [Pid + 1];
    Pid ==  MSize->
        [Pid - 1];
    true ->
        [Pid - 1, Pid + 1]
    end.

get_random_node_full(_, _, PidMap) ->
    maps:keys(PidMap).

gossip_node(?MaxRumour, rumourCount, Pid, Group, NumOfNodes, Topology) ->
    whereis(timeCalc) ! {endTime},
    gossip_node(?MaxRumour + 1, rumourCount, Pid, Group, NumOfNodes, Topology);
gossip_node(R, rumourCount, Pid, Group, NumOfNodes, Topology) ->
    receive
        { rumour, PidMap } ->
            get_random_node(Group, Pid, processIdInGroup, PidMap, Topology) ! { rumour, PidMap },
            gossip_node(R+1, rumourCount, Pid, Group, NumOfNodes, Topology)
end.

push_sum_node(S, value1, W, value2, Pid, Group, PidMap, _, Topology) ->
    Sum = S/W,
    RandomNode = get_random_node(Group, Pid, processIdInGroup, PidMap, Topology),
    RandomNode ! {S/2, W/2, PidMap},
    Sum.

push_sum_node(S, value1, W, value2, Sum, lastEstimate, C, sumCounter, Pid, Group, NumOfNodes, Topology) when C == 3 ->
    whereis(timeCalc) ! {endTime},
    push_sum_node(S, value1, W, value2, Sum, lastEstimate, C+ 1, sumCounter, Pid, Group, NumOfNodes, Topology);
push_sum_node(S, value1, W, value2, Sum, lastEstimate, C, sumCounter, Pid, Group, NumOfNodes, Topology) ->
    receive
        {NewS, NewW, PidMap} ->
            %io:format("received msg ~p", [Pid]),
            NewSum = push_sum_node(S + NewS, value1, W + NewW, value2, Pid, Group, PidMap, NumOfNodes, Topology),
            if (abs(NewSum - Sum) =< 0.000000001) ->
                push_sum_node((S + NewS)/2, value1, (W + NewW)/2, value2, NewSum, lastEstimate, C+ 1, sumCounter, Pid, Group, NumOfNodes, Topology);
            true ->
                push_sum_node((S + NewS)/2, value1, (W + NewW)/2, value2, NewSum, lastEstimate, 1, sumCounter, Pid, Group, NumOfNodes, Topology)
            end
end.

% line topology
line_topology(0, NumOfNodes, Group, PidMap, ToKill) ->
    pg:start(my_scope),
    line_topology(1, NumOfNodes, Group, PidMap, ToKill);
line_topology(CurrentNode, NumOfNodes, _, PidMap, ToKill) when CurrentNode > NumOfNodes ->
    NewPidMap = kill_full_topology_nodes(ToKill, NumOfNodes, PidMap),
    maps:get(1, NewPidMap) ! {rumour, NewPidMap},
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, NumOfNodes) end),
    register(timeCalc, Pid);
line_topology(CurrentNode, NumOfNodes, Group, PidMap, ToKill) ->
    Pid = spawn(fun() -> gossip_node(0, rumourCount, CurrentNode, Group, NumOfNodes, line) end),
    pg:join(my_scope, Group, Pid),
    line_topology(CurrentNode + 1, NumOfNodes, Group, maps:put(CurrentNode, Pid, PidMap), ToKill).

line_topology_push_sum(0, NumOfNodes, Group, PidMap, ToKill) ->
    pg:start(my_scope),
    line_topology_push_sum(1, NumOfNodes, Group, PidMap, ToKill);

line_topology_push_sum(CurrentNode, NumOfNodes, _, PidMap, ToKill) when CurrentNode > NumOfNodes ->
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, NumOfNodes) end),
    register(timeCalc, Pid),
    NewPidMap = kill_full_topology_nodes(ToKill, NumOfNodes, PidMap),
    maps:get(1, NewPidMap) ! {rumour, NewPidMap},
    maps:get(1, PidMap) ! {1, 1, PidMap};

line_topology_push_sum(CurrentNode, NumOfNodes, Group, PidMap, ToKill) ->
    Pid = spawn(fun() -> push_sum_node(CurrentNode, value1, 1, value2, CurrentNode, lastEstimate, 0, sumCounter, CurrentNode, Group, NumOfNodes, line) end),
    pg:join(my_scope, Group, Pid),
    line_topology_push_sum(CurrentNode + 1, NumOfNodes, Group, maps:put(CurrentNode, Pid, PidMap), ToKill).


%%full

kill_full_topology_nodes(0, _, PidMap) ->
    PidMap;
kill_full_topology_nodes(ToKill, N, PidMap) ->
    kill_full_topology_nodes(ToKill - 1, N, maps:remove(rand:uniform(N - 2 + 1) + 2 - 1, PidMap)).

full_topology(0, NumOfNodes, Group, PidMap, ToKill) ->
    pg:start(my_scope),
    full_topology(1, NumOfNodes, Group, PidMap, ToKill);
full_topology(CurrentNode, NumOfNodes, _, PidMap, ToKill) when CurrentNode > NumOfNodes ->
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, NumOfNodes) end),
    register(timeCalc, Pid),
    NewPidMap = kill_full_topology_nodes(ToKill, NumOfNodes, PidMap),
    maps:get(1, NewPidMap) ! {rumour, NewPidMap};
full_topology(CurrentNode, NumOfNodes, Group, PidMap, ToKill) ->
    Pid = spawn(fun() -> gossip_node(0, rumourCount, CurrentNode, Group, NumOfNodes, full) end),
    pg:join(my_scope, Group, Pid),
    full_topology(CurrentNode + 1, NumOfNodes, Group, maps:put(CurrentNode, Pid, PidMap), ToKill).
    
full_topology_push_sum(0, NumOfNodes, Group, PidMap, ToKill) ->
    pg:start(my_scope),
    full_topology_push_sum(1, NumOfNodes, Group, PidMap, ToKill);

full_topology_push_sum(CurrentNode, NumOfNodes, _, PidMap, ToKill) when CurrentNode > NumOfNodes ->
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, NumOfNodes) end),
    register(timeCalc, Pid),
    NewPidMap = kill_full_topology_nodes(ToKill, NumOfNodes, PidMap),
    maps:get(1, NewPidMap) ! {1, 1, NewPidMap};

full_topology_push_sum(CurrentNode, NumOfNodes, Group, PidMap, ToKill) ->
    Pid = spawn(fun() -> push_sum_node(CurrentNode, value1, 1, value2, CurrentNode, lastEstimate, 0, sumCounter, CurrentNode, Group, NumOfNodes, full) end),
    pg:join(my_scope, Group, Pid),
    full_topology_push_sum(CurrentNode + 1, NumOfNodes, Group, maps:put(CurrentNode, Pid, PidMap), ToKill).

%% 2D Topology

kill_2d_topology_nodes(0, _, PidMap) ->
    PidMap;
kill_2d_topology_nodes(ToKill, N, PidMap) ->
    kill_2d_topology_nodes(ToKill - 1, N, maps:remove(rand:uniform(N - 2 + 1) + 2 - 1, PidMap)).

twoD_gossip(0, _, PidMap, _) ->
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, maps:size(PidMap)) end),
    register(timeCalc, Pid),
    maps:get({1, 1}, PidMap) ! {rumour, PidMap};

twoD_gossip(N, KeyList, PidMap, Group) ->
    Key = hd(KeyList),
    Pid = spawn(fun() -> gossip_node(0, rumourCount, Key, Group, numOfNodes, "2D") end),
    pg:join(my_scope, Group, Pid),
    twoD_gossip(N-1, KeyList -- [Key], maps:put(Key, Pid, PidMap), Group).

twoD_topology(N, numOfNodes) ->
    PidMap = topologies:get_2d_grid(N, numOfNodes),
    Ids = maps:keys(PidMap),
    pg:start(my_scope),
    twoD_gossip(length(Ids), Ids, PidMap, "2D").


twoD_topology_ps(N, numOfNodes) ->
    PidMap = topologies:get_2d_grid(N, numOfNodes),
    Ids = maps:keys(PidMap),
    pg:start(my_scope),
    twoD_topology_push_sum(1, length(Ids), "2D", Ids, PidMap).
    
twoD_topology_push_sum(CurrentNode, NumOfNodes, _, _, PidMap) when CurrentNode > NumOfNodes ->
    Pid = spawn(fun() -> calc_exec_time(os:timestamp(), startTime, 0, NumOfNodes) end),
    register(timeCalc, Pid),    
    maps:get({1, 1}, PidMap) ! {1, 1, PidMap};

twoD_topology_push_sum(CurrentNode, NumOfNodes, Group, KeyList, PidMap) ->
    Key = hd(KeyList),
    Pid = spawn(fun() -> push_sum_node(CurrentNode, value1, 1, value2, CurrentNode, lastEstimate, 0, sumCounter, Key, Group, NumOfNodes, "2D" ) end),
    pg:join(my_scope, Group, Pid),
    twoD_topology_push_sum(CurrentNode + 1, NumOfNodes, Group, KeyList -- [Key],maps:put(Key, Pid, PidMap)).


calc_exec_time(_, startTime, Total, Total) ->
    io:format("all nodes reached convergence", []),
    unregister(timeCalc);
calc_exec_time(Start, startTime, Current, Total) ->
    receive
        { endTime } ->
            io:format("convergence reached - ~p", [Current]),
            io:format("total time  taken ~f seconds\n", [timer:now_diff(os:timestamp(), Start)/1000000]),
            calc_exec_time(Start, startTime, Current + 1, Total)
            %unregister(timeCalc)
        end.
    
init_process(NumOfNodes, Topology, Algorithm, ToKill) ->
    case Topology of
        "full" -> if Algorithm == "gossip" -> full_topology(0, NumOfNodes, full, maps:new(), ToKill); 
            true -> full_topology_push_sum(0, NumOfNodes, full, maps:new(), ToKill) end;
        "line" -> if Algorithm == "gossip" -> line_topology(0, NumOfNodes, line, maps:new(), ToKill) ;
            true -> line_topology_push_sum(0, NumOfNodes, line, maps:new(), ToKill) end;
        "2D" ->   if Algorithm == "gossip" -> twoD_topology(NumOfNodes, numOfNodes);
            true -> twoD_topology_ps(NumOfNodes, numOfNodes) end;
        "imp3D" -> if Algorithm == "gossip" -> twoD_topology(NumOfNodes, numOfNodes);
        true -> twoD_topology(NumOfNodes, numOfNodes) end
         end.

start() ->
    {_,[NumOfNodes,Topology,Algorithm]} = io:read("Enter the number of nodes, topology , algorithm enclosed in [,,] :"),
    {_, PercentToKill} = io:read("Enter  % of nodes to kill"),
    NodesToKill = trunc((PercentToKill/100) * NumOfNodes),
    Topology_List = ["full", "2D", "line", "imp3D"], 
    Algorithm_List = ["gossip", "push-sum"],
    case lists:member(Topology, Topology_List) of
    true ->
        case lists:member(Algorithm, Algorithm_List) of
            true ->
                init_process(NumOfNodes, Topology, Algorithm, NodesToKill),
            ok;
            false ->
                io:fwrite("Invalid Algorithm. Please enter Algorithm as gossip/push-sum \n")
            end;
    false ->
        io:fwrite("Invalid Topology. Please enter topology as full/2D/line/imp3D \n")
    end.
