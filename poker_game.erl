-module(poker_game).

-export([reset_game/1, swap_cards/1, start/0, hand_action/2]).

start() ->
    io:format(os:cmd(clear)),
    Input = io:read( "Welcome to Erlang Poker. Please enter the number of players (1-5) followed by dot and press enter >>" ), 
    Players = element(2, Input),
    {Hands, Deck} = reset_game(Players),
    {HandsAfter, _} = loop_hands({Hands, Deck}),
    {WinnerHand, HighCard, Rank} = ranker:winning_hand(HandsAfter),
    io:format("Winning hand ~p", [Rank]).

loop_hands({Hands, Deck}) -> loop_hands(Hands, [], Deck).

loop_hands([], HandsAfter, Deck) ->
    {HandsAfter, Deck};
loop_hands(Hands, HandsAfter, Deck) ->
    [Hand | Rest] = Hands,
    {HandAfter, DeckAfter} = hand_action(Hand, Deck),
    loop_hands(Rest, HandsAfter ++ [HandAfter], DeckAfter).

hand_action(Hand, Deck) ->
    [io:format("Card: ~p ~n", [X] ) || X <- Hand],
    Input = io:read( "What cards you like to change? Input the indices between '[]' followed by a dot and press enter >>" ),
    Indices = element(2, Input),
    {HandAfter, DeckAfter} = swap_cards({Hand, Indices, Deck}),
    io:format("Your hand now: ~n"),
    [io:format("Card: ~p ~n", [X] ) || X <- HandAfter],
    io:read("Type a dot and press enter to continue"),
    io:format(os:cmd(clear)),
    {HandAfter, DeckAfter}.

reset_game(Players) ->
    Deck = deck:shuffle_deck(),
    {Hands, Deck_after} = deal_hands([], Deck, Players),
    {Hands, Deck_after}.

deal_hands(Hands, Deck, 0) ->
    {Hands, Deck};    
deal_hands(Hands, Deck, Count) ->
    {Deck_after, Hand} = deck:get_hand(Deck),
    Dealt_hands = Hands ++ [Hand],
    deal_hands(Dealt_hands, Deck_after, Count-1).

swap_card(Hand, Index, Deck) ->
    Hand_ = Hand -- [lists:nth(Index, Hand)],
    {Card, Deck_after} = deck:get_card(Deck),
    NewHand = Hand_ ++ [Card],
    {NewHand, Deck_after}.

swap_cards({Hand, Indices, Deck}) -> 
    DescSortedIndices = lists:reverse(lists:sort(Indices)),
    swap_cards(Hand, DescSortedIndices, Deck).

swap_cards(Hand, [], Deck) ->
    {Hand, Deck};
swap_cards(Hand, Indices, Deck) ->
    [First | Rest] = Indices,
    {NewHand, Deck_after} = swap_card(Hand, First, Deck),
    swap_cards(NewHand, Rest, Deck_after).

