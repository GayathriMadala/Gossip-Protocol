-module(topologies).
-export([set_rows/4]).
-export([set_cols/4]).
-export([get_2d_grid/2]).
-export([get_2d_array/1]).
-export([get_2d_neighbors/2]).
-export([set_3d_grid/3]).
-export([get_3d_grid/2]).
-export([random_imp_3d_neighbor/3]).


add_n1(Ne, X, Y) ->
    if X > 1, Y > 1 ->
        lists:append(Ne, [{X-1, Y-1}]);
    true ->
        Ne
    end.
add_n2(Ne, X, Y, _) ->
    if X > 1 ->
        lists:append(Ne, [{X-1, Y}]);
    true ->
        Ne
    end.

add_n3(Ne, X, Y, N) ->
    if X > 1, Y < N ->
        lists:append(Ne, [{X-1, Y+1}]);
    true ->
        Ne
    end.

add_n4(Ne, X, Y, _) ->
    if Y > 1 ->
        lists:append(Ne, [{X, Y-1}]);
    true ->
        Ne
    end.

add_n5(Ne, X, Y, N) ->
    if Y < N ->
        lists:append(Ne, [{X, Y+1}]);
    true ->
        Ne
    end.

add_n6(Ne, X, Y, N) ->
    if X < N, Y < 1 ->
        lists:append(Ne, [{X + 1, Y - 1}]);
    true ->
        Ne
    end.
    
add_n7(Ne, X, Y, N) ->
    if X < N ->
        lists:append(Ne, [{X + 1, Y}]);
    true ->
        Ne
    end.

add_n8(Ne, X, Y, N) ->
    if X < N, Y < N ->
        lists:append(Ne, [{X + 1, Y + 1}]);
    true ->
        Ne
    end.
    
get_2d_neighbors(Pid, N) ->
    {R, C} = Pid,
    Ne = [],
    L1 = add_n1(Ne, R, C),
    L2 = add_n2(L1, R, C, N),
    L3 = add_n3(L2, R, C, N),
    L4 = add_n4(L3, R, C, N),
    L5 = add_n5(L4, R, C, N),
    L6 = add_n6(L5, R, C, N),
    L7 = add_n7(L6, R, C, N),
    L8 = add_n8(L7, R, C, N),
    lists:uniq(L8).


set_rows(I, J, N,  Map) when I == N ->
    maps:put({I, J}, 0, Map);
set_rows(I, J, N,  Map) ->
    set_rows(I + 1, J, N,  maps:put({I, J},  0, Map)).


set_cols(_, J, N, Map) when J == N + 1 ->
    Map;
set_cols(I, J, N,  Map) ->
    Map1 = set_rows(I, J, N,  Map),
    set_cols(I, J+1, N,  Map1).

get_2d_array(N) ->
    set_cols(1, 1, N,  maps:new()).

get_2d_grid(N, numOfNodes) ->
    M = trunc(math:sqrt(N)),
    io:format("~p", [M]),
    get_2d_array(M).

set_3d_grid(I, N, Map) when I == N + 1 ->
    Map;
set_3d_grid(I, N, Map) ->
     _2d = get_2d_array(N),
     %_2d,
     set_3d_grid(I + 1, N, maps:put(I, _2d, Map)).

get_3d_grid(N, dimension) ->
    set_3d_grid(1, N, maps:new()).

plane_ne1(Ne, Z, N) ->
    if Z >= 1, N >= 2, Z < N ->
        lists:append(Ne, [Z + 1]);
    true ->
        Ne
    end.

plane_ne2(Ne, Z, N) ->
    if Z == N, N > 1 ->
        lists:append(Ne, [Z - 1]);
    true ->
        Ne
    end.

plane_ne3(Ne, Z, N) ->
    if Z > 1, Z < N ->
        lists:append(Ne, [Z - 1, Z + 1]);
    true ->
        Ne
    end.


random_imp_3d_neighbor(Z, N, PidMap) ->
    X = rand:uniform(N),
    Y = rand:uniform(N),
    Ne = [],
    L1 = plane_ne1(Ne, Z, N),
    L2 = plane_ne2(L1, Z, N),
    L3 = plane_ne3(L2, Z, N),
    NewZ = lists:nth(rand:uniform(length(L3)), L3),
    io:format("~p ~p ~p\n", [X, Y, NewZ]),
    maps:get({X, Y}, maps:get(NewZ, PidMap)).
