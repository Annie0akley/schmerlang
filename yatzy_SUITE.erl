%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, Alert Logic
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2018 4:22 PM
%%%-------------------------------------------------------------------
-module(yatzy_SUITE).
-author("ajohnston").

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([roll_default_test/1, roll_keepers_ones_test/1, roll_keepers_sixes_test/1]).
-export([upper_one_score_one_test/1, upper_one_score_two_test/1, upper_one_score_three_test/1, upper_one_score_four_test/1]).
-export([upper_one_score_zero_test/1, upper_two_score_four_test/1, upper_two_score_zero_test/1, upper_three_score_nine_test/1]).
-export([upper_three_score_zero_test/1, upper_four_score_twelve_test/1, upper_four_score_zero_test/1, upper_five_score_ten_test/1]).
-export([upper_five_score_zero_test/1, upper_six_score_thirty_test/1, upper_six_score_zero_test/1]).
-export([one_pair_score_ten_test/1, one_pair_score_highest_pair_test/1, one_pair_score_zero_test/1, one_pair_score_triple_test/1]).
-export([three_of_a_kind_score_zero_test/1, three_of_a_kind_score_nine_test/1, three_of_a_kind_score_quad_test/1]).
-export([four_of_a_kind_score_zero_test/1, four_of_a_kind_score_eight_test/1, four_of_a_kind_score_quin_test/1]).
-export([two_pairs_score_zero_test/1, two_pairs_score_ten_test/1, two_pairs_score_triple_test/1, two_pairs_score_not_same_test/1]).
-export([full_house_score_zero_test/1, full_house_score_nineteen_test/1, full_house_score_eight_test/1, full_house_score_not_same_test/1]).
-export([yatzy_score_zero_test/1, yatzy_score_fifty_test/1, yatzy_score_fifty_again_test/1]).
-export([small_straight_score_zero_test/1, small_straight_score_fifteen_test/1]).
-export([large_straight_score_zero_test/1, large_straight_score_twenty_test/1]).
-export([all_filled_true_test/1, all_filled_false_test/1]).
-export([create_new_sheet_test/1, get_bonus_score_test/1, get_grand_total_score_test/1, get_all_uppers_score_test/1, get_all_lowers_score_test/1]).
-export([fill_ones_test/1, fill_twos_test/1, fill_threes_test/1, fill_fours_test/1, fill_fives_test/1, fill_sixes_test/1]).
-export([fill_one_pair_test/1, fill_three_of_a_kind_test/1, fill_four_of_a_kind_test/1, fill_two_pairs_test/1, fill_yatzy_test/1]).
-export([fill_full_house_test/1, fill_small_straight_test/1, fill_large_straight_test/1]).

all() ->
  [{group, upper_score}, {group, one_pair_score}, {group, three_of_a_kind_score}, {group, four_of_a_kind_score}, {group, two_pairs_score},
    {group, full_house_score}, {group, yatzy_score}, {group, small_straight_score}, {group, large_straight_score}, {group, get_scores},
    {group, fill_upper_scores}, {group, fill_lower_scores}].

groups() ->
  [{roll_score,
    [],
    [roll_default_test, roll_keepers_ones_test, roll_keepers_sixes_test]},
    {upper_score,
    [],
    [upper_one_score_one_test, upper_one_score_two_test, upper_one_score_three_test, upper_one_score_four_test,
      upper_one_score_zero_test, upper_two_score_four_test, upper_two_score_zero_test, upper_three_score_nine_test,
      upper_three_score_zero_test, upper_four_score_twelve_test, upper_four_score_zero_test, upper_five_score_ten_test,
      upper_five_score_zero_test, upper_six_score_thirty_test, upper_six_score_zero_test]},
    {one_pair_score,
    [],
    [one_pair_score_ten_test, one_pair_score_highest_pair_test, one_pair_score_zero_test, one_pair_score_triple_test]},
    {three_of_a_kind_score,
      [],
      [three_of_a_kind_score_zero_test, three_of_a_kind_score_nine_test, three_of_a_kind_score_quad_test]},
    {four_of_a_kind_score,
      [],
      [four_of_a_kind_score_zero_test, four_of_a_kind_score_eight_test, four_of_a_kind_score_quin_test]},
    {two_pairs_score,
      [],
      [two_pairs_score_zero_test, two_pairs_score_ten_test, two_pairs_score_triple_test, two_pairs_score_not_same_test]},
    {full_house_score,
      [],
      [full_house_score_zero_test, full_house_score_nineteen_test, full_house_score_eight_test, full_house_score_not_same_test]},
    {yatzy_score,
      [],
      [yatzy_score_zero_test, yatzy_score_fifty_test, yatzy_score_fifty_again_test]},
    {small_straight_score,
      [],
      [small_straight_score_zero_test, small_straight_score_fifteen_test]},
    {large_straight_score,
      [],
      [large_straight_score_zero_test, large_straight_score_twenty_test]},
    {get_scores,
      [],
      [all_filled_true_test, all_filled_false_test, create_new_sheet_test, get_bonus_score_test, get_grand_total_score_test,
        get_all_uppers_score_test, get_all_lowers_score_test]},
    {fill_upper_scores,
      [],
      [fill_ones_test, fill_twos_test, fill_threes_test, fill_fours_test, fill_fives_test, fill_sixes_test]},
    {fill_lower_scores,
      [],
      [fill_one_pair_test, fill_three_of_a_kind_test, fill_four_of_a_kind_test, fill_two_pairs_test, fill_yatzy_test,
        fill_full_house_test, fill_small_straight_test, fill_large_straight_test]}].

roll_default_test(_Config) -> 
  [_,_,_,_,_] = yatzy_score:roll().
roll_keepers_ones_test(_Config) -> 
  [1,1,_,_,_] = lists:sort(yatzy_score:roll([1,1])).
roll_keepers_sixes_test(_Config) -> 
  [_,6,6,6,6] = lists:sort(yatzy_score:roll([6,6,6,6])).

upper_one_score_one_test(_Config) -> 
  1 = yatzy_score:upper(1,[2,2,1,8,3]).
upper_one_score_two_test(_Config) -> 
  2 = yatzy_score:upper(1,[2,2,1,1,3]).
upper_one_score_three_test(_Config) -> 
  3 = yatzy_score:upper(1,[1,1,2,1,8]).
upper_one_score_four_test(_Config) -> 
  4 = yatzy_score:upper(1,[1,1,2,1,1]).
upper_one_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(1,[5,5,2,5,3]).
upper_two_score_four_test(_Config) -> 
  4 = yatzy_score:upper(2,[2,1,2,1,3]).
upper_two_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(2,[5,5,1,5,3]).
upper_three_score_nine_test(_Config) -> 
  9 = yatzy_score:upper(3,[1,3,3,1,3]).
upper_three_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(3,[5,5,2,5,1]).
upper_four_score_twelve_test(_Config) -> 
  12 = yatzy_score:upper(4,[1,4,4,1,4]).
upper_four_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(4,[5,5,1,5,3]).
upper_five_score_ten_test(_Config) -> 
  10 = yatzy_score:upper(5,[5,1,5,1,3]).
upper_five_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(5,[1,1,2,1,1]).
upper_six_score_thirty_test(_Config) -> 
  30 = yatzy_score:upper(6,[6,6,6,6,6]).
upper_six_score_zero_test(_Config) -> 
  0 = yatzy_score:upper(6,[5,5,1,5,3]).

one_pair_score_ten_test(_Config) -> 
  10 = yatzy_score:one_pair([1,5,2,3,5]).
one_pair_score_highest_pair_test(_Config) -> 
  12 = yatzy_score:one_pair([1,6,1,3,6]).
one_pair_score_zero_test(_Config) -> 
  0 = yatzy_score:one_pair([1,2,4,3,6]).
one_pair_score_triple_test(_Config) -> 
  8 = yatzy_score:one_pair([4,2,4,3,4]).

three_of_a_kind_score_zero_test(_Config) -> 
  0 = yatzy_score:three_of_a_kind([1,5,2,3,5]).
three_of_a_kind_score_nine_test(_Config) -> 
  9 = yatzy_score:three_of_a_kind([3,5,2,3,3]).
three_of_a_kind_score_quad_test(_Config) -> 
  6 = yatzy_score:three_of_a_kind([2,2,2,3,2]).

four_of_a_kind_score_zero_test(_Config) -> 
  0 = yatzy_score:four_of_a_kind([5,5,2,3,5]).
four_of_a_kind_score_eight_test(_Config) -> 
  8 = yatzy_score:four_of_a_kind([2,5,2,2,2]).
four_of_a_kind_score_quin_test(_Config) -> 
  12 = yatzy_score:four_of_a_kind([3,3,3,3,3]).

two_pairs_score_zero_test(_Config) -> 
  0 = yatzy_score:two_pairs([5,5,2,3,5]).
two_pairs_score_ten_test(_Config) -> 
  10 = yatzy_score:two_pairs([2,5,2,3,3]).
two_pairs_score_triple_test(_Config) -> 
  16 = yatzy_score:two_pairs([5,5,3,3,5]).
two_pairs_score_not_same_test(_Config) -> 
  0 = yatzy_score:two_pairs([5,5,5,5,5]).

full_house_score_zero_test(_Config) -> 
  0 = yatzy_score:full_house([5,5,2,3,5]).
full_house_score_nineteen_test(_Config) -> 
  19 = yatzy_score:full_house([3,5,3,3,5]).
full_house_score_eight_test(_Config) -> 
  8 = yatzy_score:full_house([1,2,2,2,1]).
full_house_score_not_same_test(_Config) -> 
  0 = yatzy_score:full_house([5,5,5,5,5]).

yatzy_score_zero_test(_Config) -> 
  0 = yatzy_score:yatzy([1,2,3,4,5]).
yatzy_score_fifty_test(_Config) -> 
  50 = yatzy_score:yatzy([1,1,1,1,1]).
yatzy_score_fifty_again_test(_Config) -> 
  50 = yatzy_score:yatzy([6,6,6,6,6]).

small_straight_score_zero_test(_Config) -> 
  0 = yatzy_score:small_straight([1,2,3,4,4]).
small_straight_score_fifteen_test(_Config) -> 
  15 = yatzy_score:small_straight([1,2,3,4,5]).

large_straight_score_zero_test(_Config) -> 
  0 = yatzy_score:large_straight([1,2,3,4,5]).
large_straight_score_twenty_test(_Config) -> 
  20 = yatzy_score:large_straight([2,3,4,5,6]).

all_filled_true_test(_Config) ->
  Sheet = #{ones => 3, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 6,
    upper_total => 46, bonus => 0, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 16,
    yatzy => 0, two_pairs => 0, full_house => 0, small_straight => 0,
    large_straight => 0, grand_total => 0},
  true = yatzy_sheet:all_filled(Sheet).

all_filled_false_test(_Config) ->
  Sheet = #{ones => empty, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 6,
    upper_total => 46, bonus => 0, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 16,
    yatzy => 0, two_pairs => 0, full_house => 0, small_straight => 0,
    large_straight => 0, grand_total => 0},
  false = yatzy_sheet:all_filled(Sheet).

create_new_sheet_test(_Config) ->
  Sheet = #{ones => empty, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 0, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty, yatzy => empty,
    two_pairs => empty, full_house => empty, small_straight => empty, large_straight => empty, grand_total => 0},
  Sheet = yatzy_sheet:new().

get_bonus_score_test(_Config) ->
  Sheet = yatzy_sheet:new(),
  0 = yatzy_sheet:get_score(bonus, Sheet).

get_grand_total_score_test(_Config) ->
  Sheet = yatzy_sheet:new(),
  0 = yatzy_sheet:get_score(grand_total, Sheet).

get_all_uppers_score_test(_Config) ->
  Sheet = #{ones => 4, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 24,
    upper_total => 65, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 0},
  4 = yatzy_sheet:get_score(ones, Sheet),
  6 = yatzy_sheet:get_score(twos, Sheet),
  9 = yatzy_sheet:get_score(threes, Sheet),
  12 = yatzy_sheet:get_score(fours, Sheet),
  10 = yatzy_sheet:get_score(fives, Sheet),
  24 = yatzy_sheet:get_score(sixes, Sheet),
  50 = yatzy_sheet:get_score(bonus, Sheet),
  65 = yatzy_sheet:get_score(upper_total, Sheet).

get_all_lowers_score_test(_Config) ->
  Sheet = #{ones => 4, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 24,
    upper_total => 65, bonus => 0, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 5, full_house => 13, small_straight => 15,
    large_straight => 20, grand_total => 0},
  4 = yatzy_sheet:get_score(one_pair, Sheet),
  9 = yatzy_sheet:get_score(three_of_a_kind, Sheet),
  8 = yatzy_sheet:get_score(four_of_a_kind, Sheet),
  0 = yatzy_sheet:get_score(yatzy, Sheet),
  5 = yatzy_sheet:get_score(two_pairs, Sheet),
  13 = yatzy_sheet:get_score(full_house, Sheet),
  15 = yatzy_sheet:get_score(small_straight, Sheet),
  20 = yatzy_sheet:get_score(large_straight, Sheet).

fill_ones_test(_Config) ->
  Sheet = #{ones => 2, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 2, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 2},
  Sheet = yatzy_sheet:fill(ones, [2,1,3,1,2], yatzy_sheet:new()),
  already_filled = yatzy_sheet:fill(ones, [2,1,3,1,2], Sheet).

fill_twos_test(_Config) ->
    %% I'd use get_score instead of creating Sheet2 like that.
    %% And I'd use yatzy_sheet:new() to create Sheet1. You should only use the exported
    %% functions of yatzy_sheet in the tests and not exploit the internal data structure.
  Sheet = #{ones => 2, twos => empty, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 2, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 2},
  Sheet2 = #{ones => 2, twos => 4, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 6, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 6},
  Sheet2 = yatzy_sheet:fill(twos, [2,1,3,1,2], Sheet).

fill_threes_test(_Config) ->
  Sheet2 = #{ones => 2, twos => 4, threes => empty, fours => empty, fives => empty, sixes => empty,
    upper_total => 6, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 6},
  Sheet3 = #{ones => 2, twos => 4, threes => 6, fours => empty, fives => empty, sixes => empty,
    upper_total => 12, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 12},
  Sheet3 = yatzy_sheet:fill(threes, [3,1,3,1,2], Sheet2).

fill_fours_test(_Config) ->
  Sheet3 = #{ones => 2, twos => 4, threes => 6, fours => empty, fives => empty, sixes => empty,
    upper_total => 12, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 12},
  Sheet4 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => empty, sixes => empty,
    upper_total => 32, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 32},
  Sheet4 = yatzy_sheet:fill(fours, [4,4,4,4,4], Sheet3).

fill_fives_test(_Config) ->
  Sheet4 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => empty, sixes => empty,
    upper_total => 32, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 32},
  Sheet5 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => empty,
    upper_total => 57, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 57},
  Sheet5 = yatzy_sheet:fill(fives, [5,5,5,5,5], Sheet4).

fill_sixes_test(_Config) ->
  Sheet5 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => empty,
    upper_total => 57, bonus => 0, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 57},
  Sheet6 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 119},
  Sheet6 = yatzy_sheet:fill(sixes, [5,6,5,6,5], Sheet5).

fill_one_pair_test(_Config) ->
  Sheet6 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => empty, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 119},
  Sheet7 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 123},
  Sheet7 = yatzy_sheet:fill(one_pair, [2,6,3,2,5], Sheet6).

fill_three_of_a_kind_test(_Config) ->
  Sheet7 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => empty, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 123},
  Sheet8 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 132},
  Sheet8 = yatzy_sheet:fill(three_of_a_kind, [3,6,3,3,5], Sheet7).

fill_four_of_a_kind_test(_Config) ->
  Sheet8 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => empty,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 132},
  Sheet9 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet9 = yatzy_sheet:fill(four_of_a_kind, [2,6,2,2,2], Sheet8).

fill_yatzy_test(_Config) ->
  Sheet9 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => empty, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet10 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet10 = yatzy_sheet:fill(yatzy, [2,6,2,2,2], Sheet9).

fill_two_pairs_test(_Config) ->
  Sheet10 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => empty, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 140},
  Sheet11 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 150},
  Sheet11 = yatzy_sheet:fill(two_pairs, [3,6,3,2,2], Sheet10).

fill_full_house_test(_Config) ->
  Sheet11 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => empty, small_straight => empty,
    large_straight => empty, grand_total => 150},
  Sheet12 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => empty,
    large_straight => empty, grand_total => 163},
  Sheet12 = yatzy_sheet:fill(full_house, [3,3,3,2,2], Sheet11).

fill_small_straight_test(_Config) ->
  Sheet12 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => empty,
    large_straight => empty, grand_total => 163},
  Sheet13 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
    large_straight => empty, grand_total => 178},
  Sheet13 = yatzy_sheet:fill(small_straight, [3,1,4,2,5], Sheet12).

fill_large_straight_test(_Config) ->
  Sheet13 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
    large_straight => empty, grand_total => 178},
  Sheet14 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
    upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
    yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
    large_straight => 20, grand_total => 198},
  Sheet14 = yatzy_sheet:fill(large_straight, [3,2,5,6,4], Sheet13).
