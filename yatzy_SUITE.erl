-module(yatzy_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0]).
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

all() ->
  [{group, upper}, {group, one_pair}, {group, three_of_a_kind}, {group, four_of_a_kind}, {group, two_pairs},
    {group, full_house}, {group, yatzy}, {group, small_straight}, {group, large_straight}].

groups() ->
  [{upper,
    [],
    [upper_one_score_one_test, upper_one_score_two_test, upper_one_score_three_test, upper_one_score_four_test,
      upper_one_score_zero_test, upper_two_score_four_test, upper_two_score_zero_test, upper_three_score_nine_test,
      upper_three_score_zero_test, upper_four_score_twelve_test, upper_four_score_zero_test, upper_five_score_ten_test,
      upper_five_score_zero_test, upper_six_score_thirty_test, upper_six_score_zero_test]},
    {one_pair,
    [],
    [one_pair_score_ten_test, one_pair_score_highest_pair_test, one_pair_score_zero_test, one_pair_score_triple_test]},
    {three_of_a_kind,
      [],
      [three_of_a_kind_score_zero_test, three_of_a_kind_score_nine_test, three_of_a_kind_score_quad_test]},
    {four_of_a_kind,
      [],
      [four_of_a_kind_score_zero_test, four_of_a_kind_score_eight_test, four_of_a_kind_score_quin_test]},
    {two_pairs,
      [],
      [two_pairs_score_zero_test, two_pairs_score_ten_test, two_pairs_score_triple_test, two_pairs_score_not_same_test]},
    {full_house,
      [],
      [full_house_score_zero_test, full_house_score_nineteen_test, full_house_score_eight_test, full_house_score_not_same_test]},
    {yatzy,
      [],
      [yatzy_score_zero_test, yatzy_score_fifty_test, yatzy_score_fifty_again_test]},
    {small_straight,
      [],
      [small_straight_score_zero_test, small_straight_score_fifteen_test]},
    {large_straight,
      [],
      [large_straight_score_zero_test, large_straight_score_twenty_test]}].

upper_one_score_one_test(_Config) -> 1 = yatzy_score:upper(1,[2,2,1,8,3]).
upper_one_score_two_test(_Config) -> 2 = yatzy_score:upper(1,[2,2,1,1,3]).
upper_one_score_three_test(_Config) -> 3 = yatzy_score:upper(1,[1,1,2,1,8]).
upper_one_score_four_test(_Config) -> 4 = yatzy_score:upper(1,[1,1,2,1,1]).
upper_one_score_zero_test(_Config) -> 0 = yatzy_score:upper(1,[5,5,2,5,3]).
upper_two_score_four_test(_Config) -> 4 = yatzy_score:upper(2,[2,1,2,1,3]).
upper_two_score_zero_test(_Config) -> 0 = yatzy_score:upper(2,[5,5,1,5,3]).
upper_three_score_nine_test(_Config) -> 9 = yatzy_score:upper(3,[1,3,3,1,3]).
upper_three_score_zero_test(_Config) -> 0 = yatzy_score:upper(3,[5,5,2,5,1]).
upper_four_score_twelve_test(_Config) -> 12 = yatzy_score:upper(4,[1,4,4,1,4]).
upper_four_score_zero_test(_Config) -> 0 = yatzy_score:upper(4,[5,5,1,5,3]).
upper_five_score_ten_test(_Config) -> 10 = yatzy_score:upper(5,[5,1,5,1,3]).
upper_five_score_zero_test(_Config) -> 0 = yatzy_score:upper(5,[1,1,2,1,1]).
upper_six_score_thirty_test(_Config) -> 30 = yatzy_score:upper(6,[6,6,6,6,6]).
upper_six_score_zero_test(_Config) -> 0 = yatzy_score:upper(6,[5,5,1,5,3]).

one_pair_score_ten_test(_Config) -> 10 = yatzy_score:one_pair([1,5,2,3,5]).
one_pair_score_highest_pair_test(_Config) -> 12 = yatzy_score:one_pair([1,6,1,3,6]).
one_pair_score_zero_test(_Config) -> 0 = yatzy_score:one_pair([1,2,4,3,6]).
one_pair_score_triple_test(_Config) -> 8 = yatzy_score:one_pair([4,2,4,3,4]).

three_of_a_kind_score_zero_test(_Config) -> 0 = yatzy_score:three_of_a_kind([1,5,2,3,5]).
three_of_a_kind_score_nine_test(_Config) -> 9 = yatzy_score:three_of_a_kind([3,5,2,3,3]).
three_of_a_kind_score_quad_test(_Config) -> 6 = yatzy_score:three_of_a_kind([2,2,2,3,2]).

four_of_a_kind_score_zero_test(_Config) -> 0 = yatzy_score:four_of_a_kind([5,5,2,3,5]).
four_of_a_kind_score_eight_test(_Config) -> 8 = yatzy_score:four_of_a_kind([2,5,2,2,2]).
four_of_a_kind_score_quin_test(_Config) -> 12 = yatzy_score:four_of_a_kind([3,3,3,3,3]).

two_pairs_score_zero_test(_Config) -> 0 = yatzy_score:two_pairs([5,5,2,3,5]).
two_pairs_score_ten_test(_Config) -> 10 = yatzy_score:two_pairs([2,5,2,3,3]).
two_pairs_score_triple_test(_Config) -> 16 = yatzy_score:two_pairs([5,5,3,3,5]).
two_pairs_score_not_same_test(_Config) -> 0 = yatzy_score:two_pairs([5,5,5,5,5]).

full_house_score_zero_test(_Config) -> 0 = yatzy_score:full_house([5,5,2,3,5]).
full_house_score_nineteen_test(_Config) -> 19 = yatzy_score:full_house([3,5,3,3,5]).
full_house_score_eight_test(_Config) -> 8 = yatzy_score:full_house([1,2,2,2,1]).
full_house_score_not_same_test(_Config) -> 0 = yatzy_score:full_house([5,5,5,5,5]).

yatzy_score_zero_test(_Config) -> 0 = yatzy_score:yatzy([1,2,3,4,5]).
yatzy_score_fifty_test(_Config) -> 50 = yatzy_score:yatzy([1,1,1,1,1]).
yatzy_score_fifty_again_test(_Config) -> 50 = yatzy_score:yatzy([6,6,6,6,6]).

small_straight_score_zero_test(_Config) -> 0 = yatzy_score:small_straight([1,2,3,4,4]).
small_straight_score_fifteen_test(_Config) -> 15 = yatzy_score:small_straight([1,2,3,4,5]).

large_straight_score_zero_test(_Config) -> 0 = yatzy_score:large_straight([1,2,3,4,5]).
large_straight_score_twenty_test(_Config) -> 20 = yatzy_score:large_straight([2,3,4,5,6]).
