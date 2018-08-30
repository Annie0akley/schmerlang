%%%-------------------------------------------------------------------
%%% @author ajohnston
%%% @copyright (C) 2018, Alert Logic
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2018 4:22 PM
%%%-------------------------------------------------------------------
-module(yatzy_score).
-author("ajohnston").

-include_lib("eunit/include/eunit.hrl").

-export([upper/2, one_pair/1, three_of_a_kind/1, four_of_a_kind/1, two_pairs/1, full_house/1, yatzy/1]).
-export([small_straight/1, large_straight/1, roll/0, roll/1]).

-spec roll() -> list().
%% roll with no input args just rolls 5 dice and returns a list of the results
roll() ->
  lists:map(fun (_) -> rand:uniform(6-1) + 1 end, lists:seq(1,5)).

-spec roll(list()) -> list().
%% roll with a "keepers" list as input arg keeps the results in the list and
%% rolls the remaining dice, returning the keepers and the new results concatenated
roll(KeeperList) ->
  KeeperList ++ lists:map(fun (_) -> rand:uniform(6-1) + 1 end, lists:seq(1,5 - length(KeeperList))).

-spec upper(integer(), list()) -> integer().
upper(Score, DiceResults) ->
  length([X || X <- DiceResults, X==Score]) * Score.

-spec one_pair(list()) -> integer().
one_pair(DiceResults) ->
  i_one_pair(lists:sort(DiceResults)).

i_one_pair([_,_,_,A,A]) -> 2 * A;
i_one_pair([_,_,A,A,_]) -> 2 * A;
i_one_pair([_,A,A,_,_]) -> 2 * A;
i_one_pair([A,A,_,_,_]) -> 2 * A;
i_one_pair([_,_,_,_,_]) -> 0.

-spec three_of_a_kind(list()) -> integer().
three_of_a_kind(DiceResults) ->
  i_three_of_a_kind(lists:sort(DiceResults)).

i_three_of_a_kind([_,_,A,A,A]) -> 3 * A;
i_three_of_a_kind([_,A,A,A,_]) -> 3 * A;
i_three_of_a_kind([A,A,A,_,_]) -> 3 * A;
i_three_of_a_kind([_,_,_,_,_]) -> 0.

-spec four_of_a_kind(list()) -> integer().
four_of_a_kind(DiceResults) ->
  i_four_of_a_kind(lists:sort(DiceResults)).

i_four_of_a_kind([_,A,A,A,A]) -> 4 * A;
i_four_of_a_kind([A,A,A,A,_]) -> 4 * A;
i_four_of_a_kind([_,_,_,_,_]) -> 0.

-spec two_pairs(list()) -> integer().
two_pairs(DiceResults) ->
  i_two_pairs(lists:sort(DiceResults)).

i_two_pairs([_,B,B,A,A]) when A /= B -> 2 * A + 2 * B;
i_two_pairs([B,B,A,A,_]) when A /= B -> 2 * A + 2 * B;
i_two_pairs([B,B,_,A,A]) when A /= B -> 2 * A + 2 * B;
i_two_pairs([_,_,_,_,_]) -> 0.

-spec full_house(list()) -> integer().
full_house(DiceResults) ->
  i_full_house(lists:sort(DiceResults)).

i_full_house([B,B,B,A,A]) when A /= B -> 2 * A + 3 * B;
i_full_house([A,A,B,B,B]) when A /= B -> 2 * A + 3 * B;
i_full_house([_,_,_,_,_]) -> 0.

-spec yatzy(list()) -> integer().
yatzy([A,A,A,A,A]) ->
  io:format("*** CONGRATULATIONS YATZY is worth 50 points ***~n",[]),
  50;
yatzy([_,_,_,_,_]) -> 0.

-spec small_straight(list()) -> integer().
small_straight(DiceResults) ->
  i_small_straight(lists:sort(DiceResults)).

i_small_straight([1,2,3,4,5]) -> 15;
i_small_straight([_,_,_,_,_]) -> 0.

-spec large_straight(list()) -> integer().
large_straight(DiceResults) ->
  i_large_straight(lists:sort(DiceResults)).

i_large_straight([2,3,4,5,6]) -> 20;
i_large_straight([_,_,_,_,_]) -> 0.

%% unit testing

roll_default_test() -> [_,_,_,_,_] = roll().
roll_keepers_ones_test() -> [1,1,_,_,_] = lists:sort(roll([1,1])).
roll_keepers_sixes_test() -> [_,6,6,6,6] = lists:sort(roll([6,6,6,6])).
upper_one_score_one_test() -> 1 = upper(1,[2,2,1,8,3]).
upper_one_score_two_test() -> 2 = upper(1,[2,2,1,1,3]).
upper_one_score_three_test() -> 3 = upper(1,[1,1,2,1,8]).
upper_one_score_four_test() -> 4 = upper(1,[1,1,2,1,1]).
upper_one_score_five_test() -> 5 = upper(1,[1,1,1,1,1]).
upper_one_score_zero_test() -> 0 = upper(1,[5,5,2,5,3]).
upper_two_score_four_test() -> 4 = upper(2,[2,1,2,1,3]).
upper_two_score_zero_test() -> 0 = upper(2,[5,5,1,5,3]).
upper_three_score_nine_test() -> 9 = upper(3,[1,3,3,1,3]).
upper_three_score_zero_test() -> 0 = upper(3,[5,5,2,5,1]).
upper_four_score_twelve_test() -> 12 = upper(4,[1,4,4,1,4]).
upper_four_score_zero_test() -> 0 = upper(4,[5,5,1,5,3]).
upper_five_score_ten_test() -> 10 = upper(5,[5,1,5,1,3]).
upper_five_score_zero_test() -> 0 = upper(5,[1,1,2,1,1]).
upper_six_score_thirty_test() -> 30 = upper(6,[6,6,6,6,6]).
upper_six_score_zero_test() -> 0 = upper(6,[5,5,1,5,3]).

one_pair_score_ten_test() -> 10 = one_pair([1,5,2,3,5]).
one_pair_score_highest_pair_test() -> 12 = one_pair([1,6,1,3,6]).
one_pair_score_zero_test() -> 0 = one_pair([1,2,4,3,6]).
one_pair_score_triple_test() -> 8 = one_pair([4,2,4,3,4]).

three_of_a_kind_score_zero_test() -> 0 = three_of_a_kind([1,5,2,3,5]).
three_of_a_kind_score_nine_test() -> 9 = three_of_a_kind([3,5,2,3,3]).
three_of_a_kind_score_quad_test() -> 6 = three_of_a_kind([2,2,2,3,2]).

four_of_a_kind_score_zero_test() -> 0 = four_of_a_kind([5,5,2,3,5]).
four_of_a_kind_score_eight_test() -> 8 = four_of_a_kind([2,5,2,2,2]).
four_of_a_kind_score_quin_test() -> 12 = four_of_a_kind([3,3,3,3,3]).

two_pairs_score_zero_test() -> 0 = two_pairs([5,5,2,3,5]).
two_pairs_score_ten_test() -> 10 = two_pairs([2,5,2,3,3]).
two_pairs_score_triple_test() -> 16 = two_pairs([5,5,3,3,5]).
two_pairs_score_not_same_test() -> 0 = two_pairs([5,5,5,5,5]).

full_house_score_zero_test() -> 0 = full_house([5,5,2,3,5]).
full_house_score_nineteen_test() -> 19 = full_house([3,5,3,3,5]).
full_house_score_eight_test() -> 8 = full_house([1,2,2,2,1]).
full_house_score_not_same_test() -> 0 = full_house([5,5,5,5,5]).

yatzy_score_zero_test() -> 0 = yatzy([1,2,3,4,5]).
yatzy_score_fifty_test() -> 50 = yatzy([1,1,1,1,1]).
yatzy_score_fifty_again_test() -> 50 = yatzy([6,6,6,6,6]).

small_straight_score_zero_test() -> 0 = small_straight([1,2,3,4,4]).
small_straight_score_fifteen_test() -> 15 = small_straight([1,2,3,4,5]).

large_straight_score_zero_test() -> 0 = large_straight([1,2,3,4,5]).
large_straight_score_twenty_test() -> 20 = large_straight([2,3,4,5,6]).
