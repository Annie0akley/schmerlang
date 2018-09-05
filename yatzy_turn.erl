%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, Alert Logic
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2018 12:05 PM
%%%-------------------------------------------------------------------
-module(yatzy_turn).
-author("ajohnston").

-include_lib("eunit/include/eunit.hrl").

-export([start/0, roll/1, roll/2, dice/1, stop/1]).
% I had to export these even though they are kinda internal
-export([first_throw/1]).
-export([second_throw/1]).
-export([third_throw/1]).

start() ->
    Pid = spawn(yatzy_turn, first_throw, [yatzy_score:roll()]),
    {ok, Pid}.

roll(TurnPid) ->
    call(TurnPid, roll).

call(To, Msg) ->
    To ! {self(), Msg},
    receive
        Res ->
            Res
    after
        4000 ->
            timeout
    end.

roll(TurnPid, Keepers) ->
    call(TurnPid, {roll, Keepers}).

dice(TurnPid) ->
    call(TurnPid, dice).

stop(TurnPid) ->
    call(TurnPid, stop).

first_throw(Dice) ->
    receive
        {From, roll} ->
            NewDice = yatzy_score:roll(),
            From ! ok,
            second_throw(NewDice);
        {From, {roll, Keepers}} ->
            case Keepers -- Dice =:= [] of
                true ->
                    NewDice = yatzy_score:roll(Keepers),
                    From ! ok,
                    second_throw(NewDice);
                false ->
                    From ! invalid_keepers,
                    first_throw(Dice)
            end;
        {From, dice} ->
            From ! Dice,
            first_throw(Dice);
        {From, stop} ->
            io:format("Throw complete~n", []),
            From ! Dice
    end.

second_throw(Dice) ->
    receive
        {From, roll} ->
            NewDice = yatzy_score:roll(),
            From ! ok,
            third_throw(NewDice);
        {From, {roll, Keepers}} ->
            case Keepers -- Dice =:= [] of
                true ->
                    NewDice = yatzy_score:roll(Keepers),
                    From ! ok,
                    third_throw(NewDice);
                false ->
                    From ! invalid_keepers,
                    second_throw(Dice)
            end;
        {From, dice} ->
            From ! Dice,
            second_throw(Dice);
        {From, stop} ->
            io:format("Throw complete~n", []),
            From ! Dice
    end.

third_throw(Dice) ->
    receive
        {From, dice} ->
            From ! Dice
    end.
