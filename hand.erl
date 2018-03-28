-module(hand).

-export([
    rank_hand/1,
    hand_to_integers/1, is_flush/1,
    is_straight/1, is_straight_flush/1,
    is_four_a_kind/1, is_full_house/1,
    is_three_of_a_kind/1, is_two_pairs/1,
    is_pair/1
    ]).

rank_hand(Hand) ->
    IntHand = hand_to_integers(Hand),
    SortHand = sort_hand(IntHand),
    SF = is_straight_flush(SortHand),
    FK = is_four_a_kind(SortHand),
    FH = is_full_house(SortHand),
    FL = is_flush(SortHand),
    ST = is_straight(SortHand),
    TK = is_three_of_a_kind(SortHand),
    TP = is_two_pairs(SortHand),
    PA = is_pair(SortHand),
    if
        SF -> royal_flush;
        FK -> four_of_a_kind;
        FH -> full_house;
        FL -> flush;
        ST -> straight;
        TK -> three_of_a_kind;
        TP -> two_pairs;
        PA -> pair;
        true -> 'high card' + lists:last(SortHand)
    end.

sort_hand(IntHand) ->
    lists:keysort(2, IntHand).

hand_to_integers(Hand) ->
    [value_and_suit_to_integer(Card) || Card <- Hand].

value_and_suit_to_integer(Card) ->
    {Suit, Value} = Card,
    IntValue = case Value of
        jack -> 11;
        queen -> 12;
        king -> 13;
        ace -> 14;
        _ -> Value
    end,
    IntSuit = case Suit of
        hearts -> 1;
        diamonds -> 2;
        clubs -> 3;
        spades -> 4
    end,
    {IntSuit, IntValue}.
        
is_straight_flush(IntHand) ->
    is_flush(IntHand) and is_straight(IntHand).

is_four_a_kind(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) and (C == D) ->
            true;
        (B == C) and (C == D) and (D == E) ->
            true;
        true -> false
    end.

is_full_house(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) and (D == E) ->
            true;
        (A == B) and (C == D) and (D == E) ->
            true;
        true -> false
    end.

is_flush(IntHand) ->
    Suits = [Suit || {Suit, _} <- IntHand],
    Flush = case Suits of
        [1,1,1,1,1] -> true;
        [2,2,2,2,2] -> true;
        [3,3,3,3,3] -> true;
        [4,4,4,4,4] -> true;
        [_,_,_,_,_] -> false
    end,
    Flush.

is_straight(IntHand) ->
    Values = [Value || {_, Value} <- IntHand],
    is_straight_values(Values).

is_straight_values(Values) ->
    [Head|Tail] = Values,
    Flush = case Tail of
        [] ->
            true;
        Tail -> 
            if
                Head == (hd(Tail) - 1) ->
                    is_straight_values(Tail);
                true -> false
            end
    end,
    Flush.

is_three_of_a_kind(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) ->
            true;
        (B == C) and (C == D) ->
            true;
        (C == D) and (D == E) ->
            true;
        true -> false
    end.
    
is_two_pairs(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (C == D) ->
            true;
        (A == B) and (D == E) ->
            true;
        (B == C) and (D == E) ->
            true;
        true -> false
    end.

is_pair(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) ->
            true;
        (B == C) ->
            true;
        (C == D) ->
            true;
        (D == E) ->
            true;
        true -> false
    end.

% % High card can be checked by looking at last card