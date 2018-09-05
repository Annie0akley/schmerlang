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

-export([start_game/1, add_score/3, get_score/2, get_score_sheet/1]).
% I had to export these even though they are kinda internal
-export([i_player/0]).
-export([i_player/1]).

start_game(Name) ->
    io:format("~nWelcome to yatzy, ~p~n",[Name]),
    Pid = spawn(yatzy_player, i_player,[]),
    register(Name, Pid).

add_score(Name, Atom, Dice) ->
    io:format("~n~p - ",[Name]),
    Name ! {add_score, Atom, Dice}.

get_score(Name, Atom) ->
    io:format("~n~p - on your score sheet - ",[Name]),
    Name ! {get_score, Atom}.

get_score_sheet(Name) ->
    io:format("~n~p - ",[Name]),
    Name ! get_scores.

i_pretty_print_map(Map) ->
    List = maps:to_list(Map),
    lists:keysort(2, List),
    lists:foreach(fun({A,B}) -> io:fwrite("|~-16s|", [io_lib:write(A)]), io:fwrite("~6s|~n", [io_lib:write(B)]) end, List).

i_player() ->
    i_player(yatzy_sheet:new()).

i_player(ScoreSheet) ->
    NewScoreSheet =
        receive
            get_scores ->
                i_pretty_print_map(ScoreSheet),
                ScoreSheet;
            {get_score, Atom} ->
                io:format("score for ~w is ~p~n",[Atom, yatzy_sheet:get_score(Atom, ScoreSheet)]),
                ScoreSheet;
            {add_score, Atom, Dice} ->
                NewSheet = yatzy_sheet:fill(Atom, Dice, ScoreSheet),
                case NewSheet of
                    already_filled ->
                        io:format("The slot for ~w is already filled, no action taken:~n",[Atom]),
                        i_pretty_print_map(ScoreSheet),
                        ScoreSheet;
                    ScoreSheet ->
                        io:format(" - no action taken:~n",[]),
                        i_pretty_print_map(ScoreSheet),
                        ScoreSheet;
                    _ ->
                        io:format("~p score added to score sheet~n",[Atom]),
                        case yatzy_sheet:all_slots_filled(NewSheet) of
                            true ->
                                io:format("  Game complete your total score is ------ ~p ------   ~n",[yatzy_sheet:get_score(grand_total, NewSheet)]);
                            false ->
                                io:format("~nNext turn~n",[])
                        end,
                        NewSheet
                end
        end,
    i_player(NewScoreSheet).
