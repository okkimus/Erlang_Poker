%%% Ranker module finds the best hand.
-module(ranker).

-export([ winning_hand/1 ]).

%%----------------------------------------------------------------------
%% Function: winning_hands/1
%% Purpose: Finds the best hand from given hands.
%% Args: List of hands you want to find the best from.
%% Returns: Tuple with {Hand, Value, Rank}. Hand is list of cards,
%%          Value is the high card of the hand, Rank is the rank of the
%%          hand (higher is better).
%%----------------------------------------------------------------------
winning_hand(Hands) ->
    Best_hands = best_ranks(Hands),
    Best_hand = best_of_the_best(Best_hands),
    int_to_value(lists:sort(Best_hand)).

%%----------------------------------------------------------------------
%% Function: best_of_the_best/1
%% Purpose: Find the best hand of equally ranked hands.
%% Args: List of best hands
%% Returns: Tuple with {Hand, Value, Rank}. Hand is list of cards,
%%          Value is the high card of the hand, Rank is the rank of the
%%          hand (higher is better).
%%----------------------------------------------------------------------
best_of_the_best(Best_hands) ->
    lists:last(lists:keysort(2, Best_hands)).

%%----------------------------------------------------------------------
%% Function: best_ranks/1
%% Purpose: Finds the best hand and equally ranked hands.
%% Args: List of hands
%% Returns: List of tuples with {Hand, Value, Rank}. Hand is list of cards,
%%          Value is the high card of the hand, Rank is the rank of the
%%          hand (higher is better).
%%----------------------------------------------------------------------
best_ranks(Hands) ->
    RankedHands = hand:rank_hands(Hands),
    SortedRankedHands = lists:keysort(4, RankedHands),
    {_, _, _, HighestRankNumber} = lists:last(SortedRankedHands),
    lists:filter(fun(A) -> is_rank(A, HighestRankNumber) end, SortedRankedHands).

%%----------------------------------------------------------------------
%% Function: is_rank/2
%% Purpose: Compares if hand is ranked as given rank.
%% Args: Hand you want to compare and RankNumber to compare against.
%% Returns: Boolean.
%%----------------------------------------------------------------------
is_rank(Hand, RankNumber) ->
    {_, _, _, HandRankNumber} = Hand,
    HandRankNumber == RankNumber.

%%----------------------------------------------------------------------
%% Function: int_to_value/1
%% Purpose: Change hands high face card value from integer to atoms.
%% Args: Hand {Hand (list of cards), IntValue (hands decider card),
%%             Rank, _}.
%% Returns: Tuple with {Hand, Value, Rank}. Hand is list of cards,
%%          Value is the high card of the hand, Rank is the rank of the
%%          hand (higher is better).
%%----------------------------------------------------------------------
int_to_value(Hand) ->
    {Hand_, IntValue, Rank, _} = Hand,
    Value = case IntValue of
        11 -> jack;
        12 -> queen;
        13 -> king;
        14 -> ace;
        _ -> IntValue
    end,
    {Hand_, Value, Rank}.