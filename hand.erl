%%% Hand module takes care of ranking the poker hands.
-module(hand).

-export([ rank_hand/1, rank_hands/1 ]).

%%----------------------------------------------------------------------
%% Function: rank_hands/1
%% Purpose: Gives ranks to the hands.
%% Args: List of hands (elements with 5 cards).
%% Returns: List of ranked hands (tuples) with {Hand (5 cards),
%%                HighCard (the decider card), HandName (atom),
%%                RankNumber (higher is better)}
%%----------------------------------------------------------------------
rank_hands(Hands) -> rank_hands(Hands, []).

rank_hands([], Ranked) ->
    Ranked;
rank_hands(Hands, Ranked) ->
    [Hand | Rest] = Hands,
    Hand_ = rank_hand(Hand),
    rank_hands(Rest, Ranked ++ [Hand_]).

%%----------------------------------------------------------------------
%% Function: rank_hand/1
%% Purpose: Gives rank to the hand.
%% Args: List of 5 cards.
%% Returns: Tuple with following info {Hand (5 cards),
%%              HighCard (the decider card), HandName (atom),
%%              RankNumber (higher is better)}
%%----------------------------------------------------------------------
rank_hand(Hand) ->
    % Change the card presentation to integers for easier comparing
    IntHand = hand_to_integers(Hand),
    % Sort the hand by the number value of card for the ranking
    SortHand = sort_hand(IntHand),
    % Try all of the hands, and get the truth value and high card (or
    % the decider card ie. two players have a pair, but which one has
    % higher pair?) of the hand
    {SFB, SFC} = is_straight_flush(SortHand),
    {FKB, FKC} = is_four_a_kind(SortHand),
    {FHB, FHC} = is_full_house(SortHand),
    {FLB, FLC} = is_flush(SortHand),
    {STB, STC} = is_straight(SortHand),
    {TKB, TKC} = is_three_of_a_kind(SortHand),
    {TPB, TPC} = is_two_pairs(SortHand),
    {PAB, PAC} = is_pair(SortHand),
    {_, HighCard} = lists:last(SortHand),
    % Check the truth values and pick the first on matching
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

%%----------------------------------------------------------------------
%% Function: hand_to_integers/1
%% Purpose: Turns hand's card into integer presentation.
%% Args: List of 5 cards.
%% Returns: List of card where suit and value is now integers.
%%----------------------------------------------------------------------
hand_to_integers(Hand) ->
    [value_and_suit_to_integer(Card) || Card <- Hand].

%%----------------------------------------------------------------------
%% Function: value_and_suit_to_integer/1
%% Purpose: Turns card into it's integer presentation.
%% Args: A card.
%% Returns: Card tuple {IntSuit, IntValue}
%%----------------------------------------------------------------------
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
        
%%----------------------------------------------------------------------
%% Function: is_straight_flush/1
%% Purpose: Figures out if given hand is a straight flush.
%% Args: Takes in a hand with cards in sorted integer presentation.
%% Returns: A tuple with {isHand?, Highestcard (decider card)}
%%----------------------------------------------------------------------        
is_straight_flush(IntHand) ->
    % Check if hand is flush
    {IsFlush, _} = is_flush(IntHand),
    % Check if hand is straight
    {IsStraight, Highest} = is_straight(IntHand),
    if
        IsFlush and IsStraight ->
            {true, Highest};
        true ->
            {false, Highest}
    end.

%% Check previous function
is_four_a_kind(IntHand) ->
    % Since the cards are sorted we should find 4 same values one after another
    % -> Two possible ways, all but E should be same or all but A should be same

    % Map the cards' values to A, B, C, D, E
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        % Compare A, B, C and D. It doesn't matter what value E has
        (A == B) and (B == C) and (C == D) ->
            {true, A};
        % Compare B, C, D and E. It doesn't matter what value A has
        (B == C) and (C == D) and (D == E) ->
            {true, B};
        true -> {false, A}
    end.

%% Check previous function
is_full_house(IntHand) ->
    % Since the cards are sorted we should find the same values one after another
    % in groups of 2 and 3 -> Two possible ways (ABC+DE) or (AB+CDE)

    % Map the cards' values to A, B, C, D, E
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    if
        (A == B) and (B == C) and (D == E) ->
            {true, A};
        (A == B) and (C == D) and (D == E) ->
            {true, B};
        true -> {false, B}
    end.

%% Check previous function
is_flush(IntHand) ->
    % Take the last card (high card)
    A = lists:nth(5, IntHand),
    % Map suits of all cards in the hand
    Suits = [Suit || {Suit, _} <- IntHand],

    % There is 4 possible ways to get flush, and last one is when theres no match.
    Flush = case Suits of
        [1,1,1,1,1] -> {true, A};
        [2,2,2,2,2] -> {true, A};
        [3,3,3,3,3] -> {true, A};
        [4,4,4,4,4] -> {true, A};
        [_,_,_,_,_] -> {false, A}
    end,
    Flush.

%% Check previous function
is_straight(IntHand) ->
    % Map all the values from hand
    Values = [Value || {_, Value} <- IntHand],
    % Check if the values are ascending by 1
    IsStraight = is_straight_values(Values),
    Highest = lists:last(IntHand),
    {IsStraight, Highest}.

%% Check previous function
%% This is a helper function for previous function is_straight/1
is_straight_values(Values) ->
    % Get the first value and the rest
    [Head|Tail] = Values,
    % Use recursion
    Straight = case Tail of
        % If theres no values to compare, we are finnished and the Values were ascending by 1
        [] ->
            true;
        % If theres still something in the tail, we do comparision    
        Tail -> 
            if
                % Take the first value of tail, substract one and compare it to head's value
                Head == (hd(Tail) - 1) ->
                    % If true, do some recusion!
                    is_straight_values(Tail);
                    % If false, it's not straight
                    true -> false
            end
    end,
    Straight.

%% Check previous function
is_three_of_a_kind(IntHand) ->
    % Since the cards are sorted we should find the same values one after another
    % in groups of 3 -> Three possible ways (ABC), (BCD) or (CDE)

    % Map values to ABCDE
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],

    % Return true if there's 3 of a kind and its value, or false (and random value)
    if
        (A == B) and (B == C) ->
            {true, A};
        (B == C) and (C == D) ->
            {true, B};
        (C == D) and (D == E) ->
            {true, C};
        true -> {false, A}
    end.
    
%% Check previous function
is_two_pairs(IntHand) ->
    % Since the cards are sorted we should find the same values one after another
    % in groups of 2 -> Three possible ways (AB+CD), (AB+DE) or (BC+DE)

    % Map values to ABCDE
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    
    % Return true and the highest pair's value, or false
    if
        (A == B) and (C == D) ->
            {true, D};
        (A == B) and (D == E) ->
            {true, E};
        (B == C) and (D == E) ->
            {true, E};
        true -> {false, A}
    end.

%% Check previous function
is_pair(IntHand) ->
    % Since the cards are sorted we should find the same values one after another
    % in groups of 2 -> Four possible ways (AB), (BC), (CD) or (DE)
    
    % Map values to (ABCDE)
    [A,B,C,D,E] = [Value || {_, Value} <- IntHand],
    
    % Return true and pairs value
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

% High card can be checked by looking at last card