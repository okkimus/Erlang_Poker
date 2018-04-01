-module(hand).

-export([ rank_hand/1, rank_hands/1 ]).

rank_hands(Hands) -> rank_hands(Hands, []).

rank_hands([], Ranked) ->
    Ranked;
rank_hands(Hands, Ranked) ->
    [Hand | Rest] = Hands,
    Hand_ = rank_hand(Hand),
    rank_hands(Rest, Ranked ++ [Hand_]).

rank_hand(Hand) ->
    IntHand = hand_to_integers(Hand),
    SortHand = sort_hand(IntHand),
    {SFB, SFC} = is_straight_flush(SortHand),
    {FKB, FKC} = is_four_a_kind(SortHand),
    {FHB, FHC} = is_full_house(SortHand),
    {FLB, FLC} = is_flush(SortHand),
    {STB, STC} = is_straight(SortHand),
    {TKB, TKC} = is_three_of_a_kind(SortHand),
    {TPB, TPC} = is_two_pairs(SortHand),
    {PAB, PAC} = is_pair(SortHand),
    {_, HighCard} = lists:last(SortHand),
    if
        SFB -> {Hand, SFC, royal_flush, 9};
        FKB -> {Hand, FKC, four_of_a_kind, 8};
        FHB -> {Hand, FHC, full_house, 7};
        FLB -> {Hand, FLC, flush, 6};
        STB -> {Hand, STC, straight, 5};
        TKB -> {Hand, TKC, three_of_a_kind, 4};
        TPB -> {Hand, TPC, two_pairs, 3};
        PAB -> {Hand, PAC, pair, 2};
        true -> {Hand, HighCard, 'high card', 1}
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
    {IsFlush, _} = is_flush(IntHand),
    {IsStraight, Highest} = is_straight(IntHand),
    if
        IsFlush and IsStraight ->
            {true, Highest};
        true ->
            {false, Highest}
    end.

is_four_a_kind(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) and (C == D) ->
            {true, A};
        (B == C) and (C == D) and (D == E) ->
            {true, B};
        true -> {false, A}
    end.

is_full_house(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) and (D == E) ->
            {true, A};
        (A == B) and (C == D) and (D == E) ->
            {true, B};
        true -> {false, B}
    end.

is_flush(IntHand) ->
    A = hd(IntHand),
    Suits = [Suit || {Suit, _} <- IntHand],
    Flush = case Suits of
        [1,1,1,1,1] -> {true, A};
        [2,2,2,2,2] -> {true, A};
        [3,3,3,3,3] -> {true, A};
        [4,4,4,4,4] -> {true, A};
        [_,_,_,_,_] -> {false, A}
    end,
    Flush.

is_straight(IntHand) ->
    Values = [Value || {_, Value} <- IntHand],
    IsStraight = is_straight_values(Values),
    Highest = lists:last(IntHand),
    {IsStraight, Highest}.

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
            {true, A};
        (B == C) and (C == D) ->
            {true, B};
        (C == D) and (D == E) ->
            {true, C};
        true -> {false, A}
    end.
    
is_two_pairs(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (C == D) ->
            {true, D};
        (A == B) and (D == E) ->
            {true, E};
        (B == C) and (D == E) ->
            {true, E};
        true -> {false, A}
    end.

is_pair(IntHand) ->
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) ->
            {true, A};
        (B == C) ->
            {true, B};
        (C == D) ->
            {true, C};
        (D == E) ->
            {true, D};
        true -> {false, A}
    end.

% % High card can be checked by looking at last card