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

-export([start_game/1, roll_dice/1, roll_dice/2, add_score/3, get_score/2]).
% I had to export these even though they are kinda internal
-export([i_player/0]).
-export([i_player/1]).

-spec start_game(atom()) -> ok.
start_game(Name) ->
  io:format("~nWelcome to yatzy, ~p~n",[Name]),
  Pid = spawn(yatzy_player, i_player,[]),
  register(Name, Pid).

-spec roll_dice(atom()) -> ok.
roll_dice(Name) ->
  io:format("~p - ",[Name]),
  Name ! roll_dice,
  ok.

-spec roll_dice(atom(), list()) -> ok.
roll_dice(Name, Keepers) ->
  io:format("~p - ",[Name]),
  Name ! {roll_dice, Keepers},
  ok.

-spec add_score(atom(), atom(), list()) -> ok.
add_score(Name, Atom, DiceResults) ->
  io:format("~n~p - ",[Name]),
  Name ! {add_score, Atom, DiceResults},
  ok.

-spec get_score(atom(), atom()) -> ok.
get_score(Name, Atom) ->
  io:format("~n~p - on your score sheet - ",[Name]),
  Name ! {get_score, Atom},
  ok.

i_none_empty(Sheet) ->
  maps:values(fun(A) ->
      if
        A == empty ->
          false
      end
    end, ok, Sheet),
  true.


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
  {NewThrows, NewScoreSheet} = receive
                    roll_dice ->
                      if
                        Throws =< 3 ->
                          io:format("throw # ~p - dice results: ~p~n", [Throws, yatzy_score:roll()]),
                          {Throws + 1, ScoreSheet};
                        true -> io:format("~nnice try but you are only allowed 3 throws~n",[]),
                          {Throws + 1, ScoreSheet}
                      end;
                    {roll_dice, Keepers} ->
                      if
                        Throws =< 3 ->
                          io:format("throw # ~p - dice results: ~p~n",[Throws, yatzy_score:roll(Keepers)]),
                          {Throws + 1, ScoreSheet};
                        true -> io:format("~nnice try but you are only allowed 3 throws~nyou have to add a score into one of the slots now~n",[]),
                          {Throws + 1, ScoreSheet}
                      end;
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
                          io:format("Latest score sheet with ~p score added:~n",[Atom]),
                          i_pretty_print_map(NewSheet),
                          %Finished = i_none_empty(NewSheet),
                          %if Finished ->
                            %io:format("Game complete your total score is ~p ~n",[yatzy_sheet:get_score(grand_total, NewSheet)])
                          %end,
                          {1, NewSheet}
                      end
                  end,
i_player({NewThrows, NewScoreSheet}).
