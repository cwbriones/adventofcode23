-module(solution).
-export([
    main/0
]).

-record(rule, {from :: integer(), to :: integer(), offset :: integer()}).
-type rule() :: #rule{}.

% ============================================
% Parsing
% ============================================
-spec endswith(binary(), binary()) -> boolean().
endswith(Bin, Suffix) ->
  N = byte_size(Suffix),
  case binary:longest_common_suffix([Bin, Suffix]) of
    N -> true;
    _ -> false
  end.

-spec trimnewline(binary()) -> binary().
trimnewline(Bin) ->
  L = bit_size(Bin)-8,
  case Bin of
    <<B:L/bitstring, "\n">> -> B;
    _ -> Bin
  end.

-spec read_all() -> list(binary()).
read_all() -> read_all([]).
read_all(Lines) ->
  case io:get_line("") of
    eof -> lists:reverse(Lines);
    Line -> read_all([trimnewline(Line)|Lines])
  end.

-spec parse(list(binary())) -> {list(integer()), list(list(rule()))}.
parse([Line|Lines]) ->
  [_|[Line2]] = binary:split(Line, <<":">>),
  Parts = binary:split(Line2, <<" ">>, [global, trim_all]),
  Seeds = lists:map(fun binary_to_integer/1, Parts),
  Mappings = parse_mappings(Lines, [], []),
  {Seeds, Mappings}.

parse_mappings([], Cur, Mappings) ->
  [[]|NonEmpty] = lists:reverse([Cur|Mappings]),
  NonEmpty;
parse_mappings([<<"">>|Lines], Cur, Mappings) ->
  parse_mappings(Lines, Cur, Mappings);
parse_mappings([Line|Lines], Cur, Mappings) ->
  case endswith(Line, <<":">>) of
    true ->
      parse_mappings(Lines, [], [lists:reverse(Cur)|Mappings]);
    false ->
      [Dst, From, Len] = lists:map(fun binary_to_integer/1, binary:split(Line, <<" ">>, [global, trim_all])),
      Rule = #rule{from=From, to=From + Len - 1, offset=Dst-From},
      parse_mappings(Lines, [Rule|Cur], Mappings)
  end.

% ============================================
% Part one
% ============================================

-spec one(list(integer()), list(list(rule()))) -> integer().
one(Seeds, Mappings) -> one(Seeds, Mappings, []).

one([], _, Dsts) ->
  lists:min(Dsts);
one([Seed|Seeds], Mappings, Dsts) ->
  Dst = apply_rules(Mappings, [], Seed),
  one(Seeds, Mappings, [Dst|Dsts]).

apply_rules([], [], Seed) -> Seed;
apply_rules([M|Ms], [], Seed) ->
  apply_rules(Ms, M, Seed);
apply_rules(Ms, [#rule{from=From, to=To, offset=Offset}|_], Seed)
  when From =< Seed andalso Seed =< To ->
    apply_rules(Ms, [], Seed + Offset);
apply_rules(Ms, [_|Rs], Seed) -> apply_rules(Ms, Rs, Seed).


% ============================================
% Part two
% ============================================
-spec seeds2(list(integer())) -> list({integer(), integer()}).
seeds2(Seeds) -> seeds2(Seeds, []).

seeds2([], Acc) ->
  lists:reverse(Acc);
seeds2([S1|[S2|Rest]], Acc) ->
  seeds2(Rest, [{S1, S1 + S2 - 1}|Acc]).

-type range() :: {integer(), integer()}.

-spec two(list(range()), list(list(rule()))) -> integer().
two(Resources, [M|Mappings]) ->
  NextResources = lists:flatmap(fun(S) -> apply_range(M, S) end, Resources),
  two(NextResources, Mappings);
two(Resources, []) -> lists:min(lists:map(fun ({X, _}) -> X end, Resources)).

-spec apply_range(list(rule()), range()) -> list(range()).
apply_range(Rules, Src) -> apply_range(Rules, [], [], [Src]).
apply_range([], Dsts, Fallthrough, []) ->
  Dsts ++ Fallthrough;
apply_range([_|Rules], Dsts, Fallthrough, []) ->
  %% apply the next rule
  apply_range(Rules, Dsts, [], Fallthrough);
apply_range(Rules = [#rule{from=RStart, to=REnd, offset=Offset}|_], Dsts, Fallthrough, [{Start, End}|Srcs])
  when RStart =< Start andalso End =< REnd ->
  %% completely overlap
  Match = {Start + Offset, End + Offset},
  apply_range(Rules, [Match|Dsts], Fallthrough, Srcs);
apply_range(Rules = [#rule{from=RStart, to=REnd, offset=Offset}|_], Dsts, Fallthrough, [{Start, End}|Srcs])
  when RStart =< Start andalso Start =< REnd ->
  %% front overlaps
  Match = {Start + Offset, REnd + Offset},
  Tail = {REnd+1, End},
  apply_range(Rules, [Match|Dsts], [Tail|Fallthrough], Srcs);
apply_range(Rules = [#rule{from=RStart, to=REnd, offset=Offset}|_], Dsts, Fallthrough, [{Start, End}|Srcs])
  when RStart =< End andalso End =< REnd ->
  %% back overlaps
  Match = {RStart + Offset, End + Offset},
  Head = {Start, RStart-1},
  apply_range(Rules, [Match|Dsts], [Head|Fallthrough], Srcs);
apply_range(Rules, Dsts, Fallthrough, [Src|Srcs]) ->
  %% miss
  apply_range(Rules, Dsts, [Src|Fallthrough], Srcs).

-spec main() -> ok.
main() ->
  io:setopts([{binary, true}]),
  {Seeds, Mappings} = parse(read_all()),
  io:format("~w~n", [one(Seeds, Mappings)]),
  io:format("~w~n", [two(seeds2(Seeds), Mappings)]),
  init:stop().
