%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, Alert Logic
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2018 4:30 PM
%%%-------------------------------------------------------------------
-module(yatzy_player).
-author("ajohnston").

-include_lib("eunit/include/eunit.hrl").

-export([new/1, fill/3, get_score/2, sheet/1]).
% I had to export these even though they are kinda internal
-export([player/0]).
-export([player/1]).

-spec new(Name::atom()) -> {ok, Pid}.
new(Name) ->
    Pid = spawn(yatzy_player, player,[]),
    register(Name, Pid),
    {ok, Pid}.

-spec fill(Name::atom(), Atom::atom(), Dice::list()) -> {ok, Score::integer()}.
fill(Name, Atom, Dice) ->
    call(Name, {fill, Atom, Dice}).

-spec get_score(Name::atom(), Atom::atom()) -> {ok, Score::integer()}.
get_score(Name, Atom) ->
    call(Name, {get_score, Atom}).

-spec sheet(Name::atom()) -> {ok, ScoreSheet::map()}.
sheet(Name) ->
    call(Name, sheet).

call(To, Msg) ->
    To ! {self(), Msg},
    receive
        Res ->
            Res
    after
        4000 ->
            timeout
    end.

player() ->
    player(yatzy_sheet:new()).

player(ScoreSheet) ->
    receive
        {From, sheet} ->
            From ! {ok, ScoreSheet},
            player(ScoreSheet);
        {From, {get_score, Atom}} ->
            From ! {ok, yatzy_sheet:get_score(Atom, ScoreSheet)},
            player(ScoreSheet);
        {From, {fill, Atom, Dice}} ->
            NewSheet = yatzy_sheet:fill(Atom, Dice, ScoreSheet),
            case NewSheet of
                {error, _Msg} ->
                    From ! {error, _Msg},
                    player(ScoreSheet);
                _ ->
                    From ! {ok, yatzy_sheet:get_score(Atom, NewSheet)},
                    player(NewSheet)
            end
    end.
