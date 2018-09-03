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

-export([start_game/1, roll_dice/1, roll_dice/2, add_score/3, get_score/2, get_score_sheet/1]).
% I had to export these even though they are kinda internal
-export([i_player/0]).
-export([i_player/1]).

start_game(Name) ->
  io:format("~nWelcome to yatzy, ~p~n",[Name]),
  Pid = spawn(yatzy_player, i_player,[]),
  register(Name, Pid).

roll_dice(Name) ->
  io:format("~p - ",[Name]),
  Name ! roll_dice.

roll_dice(Name, Keepers) ->
  io:format("~p - ",[Name]),
  Name ! {roll_dice, Keepers}.

add_score(Name, Atom, DiceResults) ->
  io:format("~n~p - ",[Name]),
  Name ! {add_score, Atom, DiceResults}.

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
  NewScoreSheet = yatzy_sheet:new(),
  io:format("This is your new score sheet:~n", []),
  i_pretty_print_map(NewScoreSheet),
  i_player({1, NewScoreSheet}).

i_player({Throws, ScoreSheet}) ->
  {NewThrows, NewScoreSheet} =
    receive
      roll_dice when Throws =< 3 ->
        io:format("throw # ~p - dice results: ~p~n", [Throws, yatzy_score:roll()]),
        {Throws + 1, ScoreSheet};
      roll_dice ->
        io:format("~nnice try but you are only allowed 3 throws~nyou have to add a score into one of the slots now~n",[]),
        {Throws + 1, ScoreSheet};
      {roll_dice, Keepers} when Throws =< 3 ->
        io:format("throw # ~p - dice results: ~p~n",[Throws, yatzy_score:roll(Keepers)]),
        {Throws + 1, ScoreSheet};
      {roll_dice, Keepers} ->
        io:format("~nnice try but you are only allowed 3 throws~nyou have to add a score into one of the slots now~n",[]),
        {Throws + 1, ScoreSheet};
      get_scores ->
        i_pretty_print_map(ScoreSheet),
        {Throws, ScoreSheet};
      {get_score, Atom} ->
        io:format("score for ~w is ~p~n",[Atom, yatzy_sheet:get_score(Atom, ScoreSheet)]),
        {Throws, ScoreSheet};
      {add_score, Atom, DiceResults} ->
        NewSheet = yatzy_sheet:fill(Atom, DiceResults, ScoreSheet),
        case NewSheet of
          already_filled ->
            io:format("The slot for ~w is already filled, no action taken:~n",[Atom]),
            i_pretty_print_map(ScoreSheet),
            {Throws, ScoreSheet};
          ScoreSheet ->
            io:format(" - no action taken:~n",[]),
            i_pretty_print_map(ScoreSheet),
            {Throws, ScoreSheet};
          _ ->
            io:format("~p score added to score sheet~n",[Atom]),
            case yatzy_sheet:all_filled(NewSheet) of
              true ->
                io:format("~~~~~ Game complete your total score is ------ ~p ------ ~~~~~ ~n",[yatzy_sheet:get_score(grand_total, NewSheet)]);
              false ->
                io:format("~nNext turn~n",[])
            end,
            {1, NewSheet}
        end
    end,
i_player({NewThrows, NewScoreSheet}).
