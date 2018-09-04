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
-export([first_throw/2]).
-export([second_throw/2]).
-export([third_throw/2]).

start() ->
    Pid = spawn(yatzy_turn, first_throw, [self(), []]),
    {ok, Pid}.

roll(Pid) ->
    Pid ! roll.

roll(Pid, Keepers) ->
    Pid ! {roll, Keepers}.

dice(Pid) ->
    Pid ! dice.

stop(Pid) ->
    Pid ! stop.

first_throw(Pid, Dice) ->
    receive
        roll ->
            NewDice = yatzy_score:roll(),
            io:format("Throw # 1 - dice results: ~p~n", [NewDice]),
            Pid ! second_throw(Pid, NewDice),
            ok;
        {roll, Keepers} ->
            case list:length(Keepers) of
                0 ->
                    NewDice = yatzy_score:roll(),
                    io:format("Throw # 1 - dice results: ~p~n", [NewDice]),
                    Pid ! second_throw(Pid, NewDice),
                    ok;
                _ ->
                    io:format("~nnice try but you can only keep dice from your previous throw and this is your first throw",[]),
                    invalid_keepers
            end;
        dice ->
            Dice;
        stop ->
            io:format("Throw complete~n", []),
            Pid ! terminate(Pid),
            Dice
    end.

second_throw(Pid, Dice) ->
    receive
        roll ->
            NewDice = yatzy_score:roll(),
            io:format("Throw # 2 - dice results: ~p~n", [NewDice]),
            Pid ! third_throw(Pid, NewDice),
            ok;
        {roll, Keepers} ->
            case Keepers -- Dice =:= [] of
                true ->
                    NewDice = yatzy_score:roll(Keepers),
                    io:format("Throw # 2 - dice results: ~p~n", [NewDice]),
                    Pid ! third_throw(Pid, NewDice),
                    ok;
                false ->
                    io:format("~nnice try but you can only keep dice from your previous throw",[]),
                    Pid ! second_throw(Pid, Dice),
                    invalid_keepers
            end;
        dice ->
            Dice;
        stop ->
            io:format("Throw complete~n", []),
            Pid ! terminate(Pid),
            Dice
    end.

third_throw(Pid, Dice) ->
    receive
        roll ->
            NewDice = yatzy_score:roll(),
            io:format("Throw # 3 - dice results: ~p~n", [NewDice]),
            Pid ! terminate(Pid),
            ok;
        {roll, Keepers} ->
            case Keepers -- Dice =:= [] of
                true ->
                    NewDice = yatzy_score:roll(Keepers),
                    io:format("Throw # 3 - dice results: ~p~n", [NewDice]),
                    Pid ! terminate(Pid),
                    ok;
                false ->
                    io:format("~nnice try but you can only keep dice from your previous throw",[]),
                    Pid ! third_throw(Pid, Dice),
                    invalid_keepers
            end;
        dice ->
            Dice;
        stop ->
            io:format("Turn complete your final results are ~p~n", [Dice]),
            Pid ! terminate(Pid),
            Dice
    end.

terminate(Pid) ->
    receive
        _ ->
            io:format("You cannot roll again~n", []),
            exit(Pid, kill)
    end.
