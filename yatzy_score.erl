-module(yatzy_score).
-export([upper/2, one_pair/1, three_of_a_kind/1, four_of_a_kind/1, two_pairs/1, full_house/1, yatzy/1, small_straight/1, large_straight/1]).

upper(Score, DiceResults) ->
  length([X || X <- DiceResults, X==Score]) * Score.


one_pair(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4 ->
      2 * E5;
    E4 == E3 ->
      2 * E4;
    E3 == E2 ->
      2 * E3;
    E2 == E1 ->
      2 * E2;
    true ->
      0
  end.

three_of_a_kind(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 == E3 ->
      3 * E5;
    E4 == E3, E3 == E2 ->
      3 * E4;
    E3 == E2, E2 == E1 ->
      3 * E3;
    true ->
      0
  end.

four_of_a_kind(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 == E3, E3 == E2 ->
      4 * E5;
    E4 == E3, E3 == E2, E2 == E1 ->
      4 * E4;
    true ->
      0
  end.

two_pairs(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 /= E3, E3 == E2 ->
      2 * E5 + 2 * E3;
    E5 == E4, E4 /= E2, E2 == E1 ->
      2 * E5 + 2 * E2;
    E4 == E3, E3 /= E2, E2 == E1 ->
      2 * E4 + 2 * E2;
    true ->
      0
  end.

full_house(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 == E3, E3 /= E2, E2 == E1 ->
      3 * E5 + 2 * E2;
    E5 == E4, E4 /= E3, E3 == E2, E2 == E1 ->
      2 * E5 + 3 * E3;
    true ->
      0
  end.

yatzy(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 == E3, E3 == E2, E2 == E1 ->
      50;
    true ->
      0
  end.

small_straight(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E1 == 1, E2 == 2, E3 == 3, E4 == 4, E5 == 5 ->
      15;
    true ->
      0
  end.

large_straight(DiceResults) ->
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E1 == 2, E2 == 3, E3 == 4, E4 == 5, E5 == 6 ->
      20;
    true ->
      0
  end.
