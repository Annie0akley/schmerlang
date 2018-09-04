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

-export([new/0, fill/3, get_score/2, all_filled/1]).

%% I'd introduce a sheet() or t() type 
-type t() :: map().

%% and a type for the slots:
-type upper_slot() :: 'ones' | 'twos' | 'threes' | 'fours' | 'fives' |'sixes'.
-type lower_slot() :: 'one_pair' | 'two_pairs' | 'full_house' | 'three_of_a_kind' | 'four_of_a_kind' | 'small_straight' | 'large_straight' | 'yatzy'.
-type derived_slot() :: 'upper_total' | 'bonus' | 'lower_total'.
-type slot() :: upper_slot() | lower_slot() | derived_slot().

-spec new() -> t().
new() ->
    #{ upper_total => 0,
        bonus => 0,
        lower_total => 0}.

-spec i_upper_value(upper_slot()) -> integer().
i_upper_value(ones) -> 1;
i_upper_value(twos) -> 2;
i_upper_value(threes) -> 3;
i_upper_value(fours) -> 4;
i_upper_value(fives) -> 5;
i_upper_value(sixes) -> 6.

i_upper_slot_type(Slot) ->
    Uppers = [ones, twos, threes, fours, fives, sixes],
    lists:member(Slot, Uppers).

i_lower_slot_type(Slot) ->
    Lowers = [one_pair, two_pairs, three_of_a_kind, four_of_a_kind, full_house, small_straight, large_straight, yatzy],
    lists:member(Slot, Lowers).

-spec fill(Slot::slot(), ResultList::list(integer()), Sheet::t()) -> t().
fill(Slot, ResultList, Sheet) ->
    case i_upper_slot_type(Slot) of
        true ->
            i_fill_uppers(Slot, i_upper_value(Slot), ResultList, Sheet);
        false ->
            case i_lower_slot_type(Slot) of
                true ->
                    i_fill_lowers(Slot, ResultList, Sheet);
                _ ->
                    io:format("No such slot as ~w ",[Slot]),
                    Sheet
            end
    end.

-spec all_filled(Sheet::map()) -> atom().
all_filled(Sheet) ->
    case maps:size(Sheet) of
        17 ->
            true;
        _ ->
            false
    end.

-spec get_score(Nums::atom(), Sheet::map()) -> integer().
get_score(grand_total, Sheet) ->
    maps:get(upper_total, Sheet) + maps:get(lower_total, Sheet) + maps:get(bonus, Sheet);
get_score(Nums, Sheet) ->
    case maps:find(Nums, Sheet) of
        error ->
            empty;
        {ok, A} ->
            A
    end.

i_fill_lowers(Lower, ResultList, Sheet) ->
    case get_score(Lower, Sheet) of
        empty ->
            Score = yatzy_score:Lower(ResultList),
            Sheet2 = i_update_lower_total(Score, Sheet),
            maps:put(Lower, Score, Sheet2);
        _ ->
            already_filled
    end.

i_fill_uppers(Nums, Num, ResultList, Sheet) ->
    case maps:find(Nums, Sheet) of
        error ->
            Score = yatzy_score:upper(Num,ResultList),
            Sheet2 = i_update_upper_totals(Score, Sheet),
            maps:put(Nums, Score, Sheet2);
        _ ->
            'already_filled'
    end.

i_update_upper_totals(Score, Sheet) ->
    UpperTotal = get_score(upper_total,Sheet),
    Bonus = get_score(bonus,Sheet),
    Sheet2 = maps:put(upper_total, UpperTotal + Score, Sheet),
    i_update_bonus(Bonus, UpperTotal + Score, Sheet2).

i_update_bonus(0, UpperTotal, Sheet) when UpperTotal >= 63 ->
    io:format("*** Upper Total reached 63 - Bonus of 50 ***~n",[]),
    maps:put(bonus, 50, Sheet);

i_update_bonus(_, _UpperTotal, Sheet) ->
    Sheet.

i_update_lower_total(Score, Sheet) ->
    LowerTotal = maps:get(lower_total,Sheet),
    maps:put(lower_total, LowerTotal + Score, Sheet).

%% unit testing

all_filled_true_test() ->
    Sheet = #{ones => 3, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 6,
        upper_total => 46, bonus => 0, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 16,
        yatzy => 0, two_pairs => 0, full_house => 0, small_straight => 0,
        large_straight => 0, grand_total => 0},
    true = all_filled(Sheet).

all_filled_false_test() ->
    Sheet = new(),
    false = all_filled(Sheet).

create_new_sheet_test() ->
    Sheet = #{upper_total => 0, bonus => 0, lower_total => 0},
    Sheet = new().

get_bonus_score_test() ->
    Sheet = new(),
    0 = get_score(bonus, Sheet).

get_lower_total_score_test() ->
    Sheet = new(),
    0 = get_score(lower_total, Sheet).

get_grand_total_score_test() ->
    Sheet = new(),
    0 = get_score(grand_total, Sheet).

get_upper_total_score_test() ->
    Sheet = new(),
    0 = get_score(upper_total, Sheet).

get_all_uppers_score_test() ->
    Sheet = #{ones => 4, twos => 6, threes => 9, fours => 12, fives => 10, sixes => 24,
        upper_total => 65, bonus => 50, lower_total => 0},
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
    Sheet = #{upper_total => 0, bonus => 0, lower_total => 3},
    Sheet = i_update_lower_total(3, new()).

i_update_upper_totals_no_bonus_test() ->
    Sheet = #{upper_total => 10, bonus => 0, lower_total => 0},
    Sheet = i_update_upper_totals(10, new()).

i_update_upper_totals_bonus_test() ->
    Sheet = #{upper_total => 48, bonus => 0, lower_total => 0},
    Sheet2 = #{upper_total => 63, bonus => 50, lower_total => 0},
    Sheet2 = i_update_upper_totals(15, Sheet).

i_fill_uppers_bonus_test() ->
    Sheet = #{fours => 8, fives => 10, sixes => 30, upper_total => 48, bonus => 0, lower_total => 0},
    Sheet2 = #{threes => 15, fours => 8, fives => 10, sixes => 30, upper_total => 63, bonus => 50, lower_total => 0},
    Sheet2 = i_fill_uppers(threes, 3,[3,3,3,3,3], Sheet),
    already_filled = i_fill_uppers(threes, 3,[3,3,3,3,3], Sheet2).


i_fill_uppers_no_bonus_test() ->
    Sheet = #{fours => 8, fives => 10, sixes => 30, upper_total => 48, bonus => 0, lower_total => 0},
    Sheet2 = #{threes => 12, fours => 8, fives => 10, sixes => 30, upper_total => 60, bonus => 0, lower_total => 0},
    Sheet2 = i_fill_uppers(threes, 3,[3,2,3,3,3], Sheet).

i_fill_all_lowers_test() ->
    Sheet = #{upper_total => 0, bonus => 0, one_pair => 4, lower_total => 4},
    Sheet2 = #{upper_total => 0, bonus => 0, one_pair => 4, two_pairs => 12, lower_total => 16},
    Sheet3 = #{upper_total => 0, bonus => 0, one_pair => 4, four_of_a_kind => 20, two_pairs => 12, lower_total => 36},
    Sheet4 = #{upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
        two_pairs => 12, lower_total => 42},
    Sheet5 = #{upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
        yatzy => 50, two_pairs => 12, lower_total => 92},
    Sheet6 = #{upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
        yatzy => 50, two_pairs => 12, full_house => 19, lower_total => 111},
    Sheet7 = #{upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
        yatzy => 50, two_pairs => 12, full_house => 19, small_straight => 15, lower_total => 126},
    Sheet8 = #{upper_total => 0, bonus => 0, one_pair => 4, three_of_a_kind => 6, four_of_a_kind => 20,
        yatzy => 50, two_pairs => 12, full_house => 19, small_straight => 15,
        large_straight => 20, lower_total => 146},
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
    Sheet = #{ones => 2, upper_total => 2, bonus => 0, lower_total => 0},
    Sheet2 = #{ones => 2, twos => 4, upper_total => 6, bonus => 0, lower_total => 0},
    Sheet3 = #{ones => 2, twos => 4, threes => 6, upper_total => 12, bonus => 0, lower_total => 0},
    Sheet4 = #{ones => 2, twos => 4, threes => 6, fours => 20, upper_total => 32, bonus => 0, lower_total => 0},
    Sheet5 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, upper_total => 57, lower_total => 0, bonus => 0},
    Sheet6 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, lower_total => 0},
    Sheet7 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, lower_total => 4},
    Sheet8 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, lower_total => 13},
    Sheet9 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        lower_total => 21},
    Sheet10 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        lower_total => 21, yatzy => 0},
    Sheet11 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        yatzy => 0, two_pairs => 10, lower_total => 31},
    Sheet12 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        yatzy => 0, two_pairs => 10, full_house => 13, lower_total => 44},
    Sheet13 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
        lower_total => 59},
    Sheet14 = #{ones => 2, twos => 4, threes => 6, fours => 20, fives => 25, sixes => 12,
        upper_total => 69, bonus => 50, one_pair => 4, three_of_a_kind => 9, four_of_a_kind => 8,
        yatzy => 0, two_pairs => 10, full_house => 13, small_straight => 15,
        large_straight => 20, lower_total => 79},
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
