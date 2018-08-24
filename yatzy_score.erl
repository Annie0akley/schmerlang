-module(yatzy_score).
-export([upper/2, one_pair/1, three_of_a_kind/1, four_of_a_kind/1, two_pairs/1, full_house/1, yatzy/1, small_straight/1, large_straight/1]).

%% As a minor style thing I would probably use Dice instead of DiceResults.
upper(Score, DiceResults) ->
    % the std indent in Erlang is 4 due to the widespread use of the Emacs setup that
    % comes with the OTP distribution
    length([X || X <- DiceResults, X==Score]) * Score.
    % this solution is good and has variations to the same effect:
    % lists:sum( [X || X -> DiceResults, X == Score} ).
    % The sum version has a slight advantage over length as it reads more like what you
    % would say: you get the sum of the dice that matches the eyes for the slot you want
    % to fill. Or something to that effect.
    % One can also go totally functional and do this:
    % lists:sum( lists:filter( fun (Eyes) -> Eyes == Score end, DiceResults ) ).
    % That would look nicer in Elixir, but the true elegance of that would only really
    % show in Haskell.

one_pair(DiceResults) ->
    % if statements are really crappy in Erlang, 'cause they didn't add the else, which is 
    % an absolute must in a functional language, so this logic is better done with a case 
    % statement. 
    % I have never seen one_pair done with pattern matching before, but I like it.
    % Just goes to show that you can always learn something new.
    % And one can exploit a few things about the pattern matching to make the code really
    % nice.
    case lists:sort(DiceResults) of
        [_, _ , _, E, E] ->
            2 * E;
        [_, _, E, E, _] ->
            2 * E;
        [_, E, E, _, _] ->
            2 * E;
        [E, E, _, _, _] ->
            2 * E;
        _ ->
            0
    end.

three_of_a_kind(DiceResults) ->
    %% the same approach as for one_pair should be applied here
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
    %% the same approach as for one_pair should be applied here
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
    %% the same approach as for one_pair should be applied here
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
    %% the same approach as for one_pair should be applied here
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
    %% the same approach as for one_pair should be applied here
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E5 == E4, E4 == E3, E3 == E2, E2 == E1 ->
      50;
    true ->
      0
  end.

small_straight(DiceResults) ->
    %% the same approach as for one_pair should be applied here
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E1 == 1, E2 == 2, E3 == 3, E4 == 4, E5 == 5 ->
      15;
    true ->
      0
  end.

large_straight(DiceResults) ->
    %% the same approach as for one_pair should be applied here
  [E1|[E2|[E3|[E4|[E5]]]]] = lists:sort(DiceResults),
  if
    E1 == 2, E2 == 3, E3 == 4, E4 == 5, E5 == 6 ->
      20;
    true ->
      0
  end.
