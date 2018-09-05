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
    % no need to store the Pid of the creator here, better to do the call thing from the
    % book and treat the turn as a thing you interact with using the API.
    % On the higher level it is bad to tie processes together without using a monitor or
    % link - that wil lead to things getting weird errors because a process has die and
    % they don't know about it.
    Pid = spawn(yatzy_turn, first_throw, [self(), []]),
    {ok, Pid}.

% changing to the call thingy from the book this one should be:
% roll(TurnPid) ->
%     call(TurnPid, {self(), roll}).
roll(Pid) ->
    Pid ! roll.
%
% And call should be like: 
call(To, Msg) ->
    To ! {self(), Msg},
    receive
        Res ->
            Res
    after
        4000 ->
            timeout
    end.

roll(Pid, Keepers) ->
    Pid ! {roll, Keepers}.

dice(Pid) ->
    Pid ! dice.

stop(Pid) ->
    Pid ! stop.

%% As stated above just drop the Pid and use the call approach.
first_throw(Pid, Dice) ->
    receive
        {From, roll} ->
            NewDice = yatzy_score:roll(),
            io:format("Throw # 1 - dice results: ~p~n", [NewDice]),
            % This is not the way to do it. You are sending the result of
            % second_throw(Pid, NewDice) back to the Pid itself
            %Pid ! second_throw(Pid, NewDice),
            %ok;
            % the Erlang way of doing it is like this:
            From ! ok,
            second_throw(NewDice);    
        {From, {roll, Keepers}} ->
            % Pretty sure this won't work. You need to check that all the Keepers are part
            % of the Dice and then only throw the remainding dice.
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
        {From, dice} ->
            From ! Dice,
            first_roll(Dice);
        {From, stop} ->
            io:format("Throw complete~n", []),
            %% You don't need to call terminate directly, just don't call a function on
            %% the end and the process will die nicely.
            %Pid ! terminate(Pid),
            %Dice
            From ! Dice
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
