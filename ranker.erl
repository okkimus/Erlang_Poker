-module(ranker).

-export([ winning_hand/1, best_ranks/1 ]).

winning_hand(Hands) ->
    Best_hands = best_ranks(Hands),
    Best_hand = best_of_the_best(Best_hands),
    int_to_value(Best_hand).

best_of_the_best(Best_hands) ->
    lists:last(lists:keysort(2, Best_hands)).

best_ranks(Hands) ->
    RankedHands = hand:rank_hands(Hands),
    SortedRankedHands = lists:keysort(3, RankedHands),
    {_, _, _, HighestRankNumber} = lists:last(SortedRankedHands),
    lists:filter(fun(A) -> is_rank(A, HighestRankNumber) end, SortedRankedHands).

is_rank(Hand, RankNumber) ->
    {_, _, _, HandRankNumber} = Hand,
    HandRankNumber == RankNumber.

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