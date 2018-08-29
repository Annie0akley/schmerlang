%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2018 4:30 PM
%%%-------------------------------------------------------------------
-module(yatzy_player).
-author("ajohnston").

-export([start/1, add_score/3, get_score/2]).
-export([player/0]).
-export([player/1]).

start(Name) ->
  Pid = spawn(yatzy_player, player,[]),
  register(Name, Pid),
  io:format("~nWelcome to yatzy, ~p~n",[Name]).

add_score(Name, Atom, DiceResults) ->
  io:format("~n~p - adding ~p to your score sheet - ",[Name, Atom]),
  Name ! {add_score, Atom, DiceResults}.

get_score(Name, Atom) ->
  io:format("~n~p - on your score sheet - ",[Name]),
  Name ! {get_score, Atom}.

player() ->
  NewScoreSheet = yatzy_sheet:new(),
  io:format("This is your new score sheet:~n~p~n~n",[NewScoreSheet]),
  player(NewScoreSheet).

player(ScoreSheet) ->
  NewScoreSheet = receive
                    {get_score, Atom} ->
                      io:format("score for ~w is ~p~n~n",[Atom, yatzy_sheet:get_score(Atom, ScoreSheet)]),
                      ScoreSheet;
                    {add_score, Atom, DiceResults} ->
                      NewSheet = yatzy_sheet:fill(Atom, DiceResults, ScoreSheet),
                      case NewSheet of
                        already_filled ->
                          io:format("The slot for ~w is already filled, no action taken:~n~p~n~n",[Atom, ScoreSheet]),
                          ScoreSheet;
                        _ ->
                          io:format("Updated score sheet with ~w:~n~p~n~n",[Atom, NewSheet]),
                          NewSheet
                      end
                  end,
player(NewScoreSheet).
