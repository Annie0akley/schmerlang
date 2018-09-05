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
-export([i_player/0]).
-export([i_player/1]).

new(Name) ->
    Pid = spawn(yatzy_player, i_player,[]),
    register(Name, Pid),
    {ok, Pid}.

call(To, Msg) ->
    To ! {self(), Msg},
    receive
        Res ->
            Res
    after
        4000 ->
            timeout
    end.

fill(Name, Atom, Dice) ->
    call(Name, {fill, Atom, Dice}).

get_score(Name, Atom) ->
    call(Name, {get_score, Atom}).

sheet(Name) ->
    call(Name, sheet).

i_player() ->
    i_player(yatzy_sheet:new()).

i_player(ScoreSheet) ->
    receive
        {From, sheet} ->
            From ! {ok, ScoreSheet},
            i_player(ScoreSheet);
        {From, {get_score, Atom}} ->
            From ! {ok, yatzy_sheet:get_score(Atom, ScoreSheet)},
            i_player(ScoreSheet);
        {From, {fill, Atom, Dice}} ->
            NewSheet = yatzy_sheet:fill(Atom, Dice, ScoreSheet),
            case NewSheet of
                already_filled ->
                    From ! {error, already_filled},
                    i_player(ScoreSheet);
                ScoreSheet ->
                    From ! {error, no_action},
                    i_player(ScoreSheet);
                _ ->
                    From ! {ok, yatzy_sheet:get_score(Atom, NewSheet)},
                    i_player(NewSheet)
            end
    end.
