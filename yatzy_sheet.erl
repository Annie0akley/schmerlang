%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, Alert Logic
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2018 4:22 PM
%%%-------------------------------------------------------------------
-module(yatzy_sheet).
-author("ajohnston").

-include_lib("eunit/include/eunit.hrl").
-export([new/0, fill/3, get_score/2]).

%% I'd introduce a sheet() or t() type 
-type t() :: map().

%% and a type for the slots:
-type upper_slot() :: 'ones' | 'twos' | 'sixes'.
-type lower_slot() :: 'one_pair' | 'yatzy'.
-type derived_slot() :: 'upper_total' | 'bonus' | 'grand_total'.
-type slot() :: upper_slot() | lower_slot() | derived_slot().

-spec new() -> t(). 
new() ->
    % just add the non-fillable slots and use maps:get/3 with 'empty' as the default
    % value. 
    #{ upper_total => 0,
       bonus => 0,
       grand_total => 0}.
    % and then a small optimisation question: would it be easier if you tracked
    % lower_total instead of grand_total and just calculated the grand_total every time?

-spec fill(atom(), list(), map()) -> map().
fill(ones, ResultList, Sheet) ->
    % this is can be done a bit more to the point if you create a function 
    % `upper_value(upper_slot()) -> 1..6.`
    % and then write the code from i_fill_uppers right here.
    % In the score module the i_ functions were more needed due to the need to have a
    % sorted list. In this module you are much better served by creating helper functions
    % and avoid the i_ functions.
  i_fill_uppers(ones, 1, ResultList, Sheet);

fill(twos, ResultList, Sheet) ->
  i_fill_uppers(twos, 2, ResultList, Sheet);

fill(threes, ResultList, Sheet) ->
  i_fill_uppers(threes, 3, ResultList, Sheet);

fill(fours, ResultList, Sheet) ->
  i_fill_uppers(fours, 4, ResultList, Sheet);

fill(fives, ResultList, Sheet) ->
  i_fill_uppers(fives, 5, ResultList, Sheet);

fill(sixes, ResultList, Sheet) ->
  i_fill_uppers(sixes, 6, ResultList, Sheet);

fill(one_pair, ResultList, Sheet) ->
  i_fill_lowers(one_pair, ResultList, Sheet);

fill(three_of_a_kind, ResultList, Sheet) ->
  i_fill_lowers(three_of_a_kind, ResultList, Sheet);

fill(four_of_a_kind, ResultList, Sheet) ->
  i_fill_lowers(four_of_a_kind, ResultList, Sheet);

fill(yatzy, ResultList, Sheet) ->
  i_fill_lowers(yatzy, ResultList, Sheet);

fill(two_pairs, ResultList, Sheet) ->
  i_fill_lowers(two_pairs, ResultList, Sheet);

fill(full_house, ResultList, Sheet) ->
  i_fill_lowers(full_house, ResultList, Sheet);

fill(small_straight, ResultList, Sheet) ->
  i_fill_lowers(small_straight, ResultList, Sheet);

fill(large_straight, ResultList, Sheet) ->
  i_fill_lowers(large_straight, ResultList, Sheet).


i_fill_lowers(Lower, ResultList, Sheet) ->
    % this is where you can use maps:get(Lower, Sheet, empty) and avoid having to prefill
    % the map.
    % Or... and that is behaps even better you can pattern match on the Lower key being
    % in the map - if so it is 'already_filled'.
  case maps:get(Lower, Sheet) of
    empty ->
      Score = i_get_lower_score(Lower, ResultList),
      Sheet2 = i_update_total(Score, Sheet),
      maps:put(Lower, Score, Sheet2);
    _ ->
          % only use '' around atoms when they need that to be atoms or in type specs.
      already_filled
  end.

i_get_lower_score(Lower, ResultList) ->
    %% there is a little trick in Erlang that allows you to call
    %% yatzy_score:Lower(ResultList) and it will just work ;-)
  case Lower of
    one_pair -> yatzy_score:one_pair(ResultList);
    three_of_a_kind -> yatzy_score:three_of_a_kind(ResultList);
    four_of_a_kind -> yatzy_score:four_of_a_kind(ResultList);
    yatzy -> yatzy_score:yatzy(ResultList);
    two_pairs -> yatzy_score:two_pairs(ResultList);
    full_house -> yatzy_score:full_house(ResultList);
    small_straight -> yatzy_score:small_straight(ResultList);
    large_straight -> yatzy_score:large_straight(ResultList)
  end.

i_fill_uppers(Nums, Num, ResultList, Sheet) ->
  case maps:get(Nums, Sheet) of
    empty ->
      Score = yatzy_score:upper(Num,ResultList),
      Sheet2 = i_update_upper_totals(Score, Sheet),
      maps:put(Nums, Score, Sheet2);
    _ ->
      'already_filled'
  end.


%% This function could use a bit more pattern matching.
i_update_upper_totals(Score, #{ bonus := 50,
                                upper_total := UpperTotal,
                                grand_total := GrandTotal } = Sheet) ->
    Sheet#{ upper_total => UpperTotal + Score,
            grand_total => GrandTotal + Score };
% figure out how to do the rest in this style...
i_update_upper_totals(Score, Sheet) ->
  UpperTotal = maps:get(upper_total,Sheet),
  GrandTotal = maps:get(grand_total,Sheet),
  Bonus = maps:get(bonus,Sheet),
  Sheet2 = maps:put(upper_total, UpperTotal + Score, Sheet),
  % never use if - case is the way to go!!!!
  if
    UpperTotal >= 63 - Score, Bonus == 0 ->
      Sheet3 = maps:put(bonus, 50, Sheet2),
      maps:put(grand_total, 50 + GrandTotal + Score, Sheet3);
    true ->
      maps:put(grand_total, GrandTotal + Score, Sheet2)
  end.

i_update_total(Score, Sheet) ->
  GrandTotal = maps:get(grand_total,Sheet),
  maps:put(grand_total, GrandTotal + Score, Sheet).

-spec get_score(atom(), map()) -> integer().
get_score(Nums, Sheet) ->
  maps:get(Nums, Sheet).

%% unit testing

create_new_sheet_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
  upper_total => 0, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
  yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
  large_straight => empty, grand_total => 0},
  Sheet = new().

get_bonus_score_test() ->
  Sheet = new(),
  0 = get_score(bonus, Sheet).

get_grand_total_score_test() ->
  Sheet = new(),
  0 = get_score(grand_total, Sheet).

get_all_uppers_score_test() ->
  Sheet = #{ones => 4, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 24,
    upper_total => 65, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 0},
  4 = get_score(ones, Sheet),
  6 = get_score(twos, Sheet),
  9 = get_score(threes, Sheet),
  12 = get_score(fours, Sheet),
  10 = get_score(fives, Sheet),
  24 = get_score(sixes, Sheet),
  50 = get_score(bonus, Sheet),
  65 = get_score(upper_total, Sheet).

get_all_lowers_score_test() ->
  Sheet = #{ones => 4, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 24,
    upper_total => 65, bonus => 0, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 5, full_house => 13, small_straight => 15,
    large_straight => 20, grand_total => 0},
  4 = get_score(one_pair, Sheet),
  9 = get_score(three_of_a_kind, Sheet),
  8 = get_score(four_of_a_kind, Sheet),
  0 = get_score(yatzy, Sheet),
  5 = get_score(two_pairs, Sheet),
  13 = get_score(full_house, Sheet),
  15 = get_score(small_straight, Sheet),
  20 = get_score(large_straight, Sheet).

i_update_total_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 3},
  Sheet = i_update_total(3, new()).

i_update_upper_totals_no_bonus_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 10, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 10},
  Sheet = i_update_upper_totals(10, new()).

i_update_upper_totals_bonus_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => 8, fives => 10, sixes => 30,
    upper_total => 48, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 48},
  Sheet2 = #{ones => empty, twos => empty, threes => empty, fours => 8, fives => 10, sixes => 30,
    upper_total => 63, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 113},
  Sheet2 = i_update_upper_totals(15, Sheet).

i_fill_uppers_bonus_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => 8, fives => 10, sixes => 30,
    upper_total => 48, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 48},
  Sheet2 = #{ones => empty, twos => empty, threes => 15, fours => 8, fives => 10, sixes => 30,
    upper_total => 63, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 113},
  Sheet2 = i_fill_uppers(threes, 3,[3,3,3,3,3], Sheet),
  already_filled = i_fill_uppers(threes, 3,[3,3,3,3,3], Sheet2).


i_fill_uppers_no_bonus_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => 8, fives => 10, sixes => 30,
    upper_total => 48, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 48},
  Sheet2 = #{ones => empty, twos => empty, threes => 12, fours => 8, fives => 10, sixes => 30,
    upper_total => 60, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 60},
  Sheet2 = i_fill_uppers(threes, 3,[3,2,3,3,3], Sheet).

i_get_lower_score_one_pair_test() -> 10 = i_get_lower_score(one_pair, [1,5,3,4,5]).
i_get_lower_score_three_of_a_kind_test() -> 15 = i_get_lower_score(three_of_a_kind, [1,5,5,4,5]).
i_get_lower_score_four_of_a_kind_test() -> 4 = i_get_lower_score(four_of_a_kind, [1,1,1,1,5]).
i_get_lower_score_yatzy_test() -> 50 = i_get_lower_score(yatzy, [1,1,1,1,1]).
i_get_lower_score_two_pairs_test() -> 16 = i_get_lower_score(two_pairs, [3,5,3,4,5]).
i_get_lower_score_full_house_test() -> 14 = i_get_lower_score(full_house, [1,1,4,4,4]).
i_get_lower_score_small_straight_test() -> 15 = i_get_lower_score(small_straight, [1,2,3,5,4]).
i_get_lower_score_large_straight_test() -> 20 = i_get_lower_score(large_straight, [2,5,3,4,6]).

i_fill_all_lowers_test() ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 4},
  Sheet2 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => 12, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 16},
  Sheet3 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => 20,
    yatzy => empty, two_pairs => 12, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 36},
  Sheet4 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
    yatzy => empty, two_pairs => 12, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 42},
  Sheet5 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
    yatzy => 50, two_pairs => 12, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 92},
  Sheet6 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
    yatzy => 50, two_pairs => 12, full_house => 19, small_straight => empty,
    large_straight => empty, grand_total => 111},
  Sheet7 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
    yatzy => 50, two_pairs => 12, full_house => 19, small_straight => 15,
    large_straight => empty, grand_total => 126},
  Sheet8 = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
    yatzy => 50, two_pairs => 12, full_house => 19, small_straight => 15,
    large_straight => 20, grand_total => 146},
  Sheet = i_fill_lowers(one_pair, [1,2,2,5,1], new()),
  Sheet2 = i_fill_lowers(two_pairs, [4,4,2,2,5], Sheet),
  Sheet3 = i_fill_lowers(four_of_a_kind, [5,5,2,5,5], Sheet2),
  Sheet4 = i_fill_lowers(three_of_a_kind, [4,2,2,2,5], Sheet3),
  Sheet5 = i_fill_lowers(yatzy, [5,5,5,5,5], Sheet4),
  Sheet6 = i_fill_lowers(full_house, [2,2,5,5,5], Sheet5),
  Sheet7 = i_fill_lowers(small_straight, [1,4,3,5,2], Sheet6),
  Sheet8 = i_fill_lowers(large_straight, [5,4,3,2,6], Sheet7),
  already_filled = i_fill_lowers(large_straight, [5,4,3,2,6], Sheet8).

fill_all_test() ->
  Sheet = #{ones => 2, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 2, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 2},
  Sheet2 = #{ones => 2, twos => 4, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 6, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 6},
  Sheet3 = #{ones => 2, twos => 4, threes => 6, fours => empty, fives => empty, sixes => empty,
    upper_total => 12, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 12},
  Sheet4 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => empty, sixes => empty,
    upper_total => 32, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 32},
  Sheet5 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => empty,
    upper_total => 57, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 57},
  Sheet6 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 119},
  Sheet7 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 123},
  Sheet8 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 132},
  Sheet9 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet10 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet11 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 150},
  Sheet12 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => empty,
    large_straight => empty, grand_total => 163},
  Sheet13 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
    large_straight => empty, grand_total => 178},
  Sheet14 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
    large_straight => 20, grand_total => 198},
  Sheet = fill(ones, [2,1,3,1,2], new()),
  already_filled = fill(ones, [2,1,3,1,2], Sheet),
  Sheet2 = fill(twos, [2,1,3,1,2], Sheet),
  Sheet3 = fill(threes, [3,1,3,1,2], Sheet2),
  Sheet4 = fill(fours, [4,4,4,4,4], Sheet3),
  Sheet5 = fill(fives, [5,5,5,5,5], Sheet4),
  Sheet6 = fill(sixes, [5,6,5,6,5], Sheet5),
  Sheet7 = fill(one_pair, [2,6,3,2,5], Sheet6),
  Sheet8 = fill(three_of_a_kind, [3,6,3,3,5], Sheet7),
  Sheet9 = fill(four_of_a_kind, [2,6,2,2,2], Sheet8),
  Sheet10 = fill(yatzy, [2,6,2,2,2], Sheet9),
  Sheet11 = fill(two_pairs, [3,6,3,2,2], Sheet10),
  Sheet12 = fill(full_house, [3,3,3,2,2], Sheet11),
  Sheet13 = fill(small_straight, [3,1,4,2,5], Sheet12),
  Sheet14 = fill(large_straight, [3,2,5,6,4], Sheet13).
