%%% Poker_game module takes care of creating the game, handling its state and prompting for user action.
-module(poker_game).

-export([start/0]).

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: Starts the interactive poker game.
%%----------------------------------------------------------------------
start() ->
    io:format(os:cmd(clear)),
    Input = io:read( "Welcome to Erlang Poker. Please enter the number of players (1-5) followed by dot and press enter >>" ), 
    Players = element(2, Input),
    {Hands, Deck} = reset_game(Players),
    {HandsAfter, _} = loop_hands({Hands, Deck}),
    {WinnerHand, _, Rank} = ranker:winning_hand(HandsAfter),
    io:format("Winning hand\n ~p \n ~p \n", [lists:keysort(2, WinnerHand), Rank]).

%%----------------------------------------------------------------------
%% Function: loop_hands/1
%% Purpose: Goes trough the hands and prompts for user action.
%% Args: Tuple with a list of Hands and the Deck used.
%% Returns: Tuple with a list of Hands and Deck after the looping
%%----------------------------------------------------------------------
loop_hands({Hands, Deck}) -> loop_hands(Hands, [], Deck).

loop_hands([], HandsAfter, Deck) ->
    {HandsAfter, Deck};
loop_hands(Hands, HandsAfter, Deck) ->
    [Hand | Rest] = Hands,
    {HandAfter, DeckAfter} = hand_action(Hand, Deck),
    loop_hands(Rest, HandsAfter ++ [HandAfter], DeckAfter).

%%----------------------------------------------------------------------
%% Function: rank_hands/2
%% Purpose: Prints the hand for user and prompts for action. Changes
%%          the cards user wants to change and outputs the hand.
%% Args: Hand (list of 5 cards) and Deck
%% Returns: The hand and the deck after user action.
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: reset_game/1
%% Purpose: Creates a shuffled deck and deals cards for players.
%% Args: Number of players
%% Returns: Hands and Deck after dealing the hands
%%----------------------------------------------------------------------
reset_game(Players) ->
    Deck = deck:shuffle_deck(),
    {Hands, Deck_after} = deal_hands([], Deck, Players),
    {Hands, Deck_after}.

%%----------------------------------------------------------------------
%% Function: deal_hands/3
%% Purpose: Deals hands (lists of 5 cards).
%% Args: List of Hands (empty list), Deck and Count of how many hands.
%% Returns: List of hands.
%%----------------------------------------------------------------------
deal_hands(Hands, Deck, 0) ->
    {Hands, Deck};    
deal_hands(Hands, Deck, Count) ->
    {Deck_after, Hand} = deck:get_hand(Deck),
    Dealt_hands = Hands ++ [Hand],
    deal_hands(Dealt_hands, Deck_after, Count-1).

%%----------------------------------------------------------------------
%% Function: swap_card/3
%% Purpose: Swaps a card from a hand. (Helper function for swap_cards.)
%% Args: Hand, Index of the card to swap, Deck.
%% Returns: Hand and deck after swapping one card.
%%----------------------------------------------------------------------
swap_card(Hand, Index, Deck) ->
    Hand_ = Hand -- [lists:nth(Index, Hand)],
    {Card, Deck_after} = deck:get_card(Deck),
    NewHand = Hand_ ++ [Card],
    {NewHand, Deck_after}.

%%----------------------------------------------------------------------
%% Function: swap_cards/3
%% Purpose: Swaps cards from a hand.
%% Args: Hand, Indices of the cards to swap, Deck.
%% Returns: Hand and deck after swapping the cards.
%%----------------------------------------------------------------------
swap_cards({Hand, Indices, Deck}) -> 
    DescSortedIndices = lists:reverse(lists:sort(Indices)),
    swap_cards(Hand, DescSortedIndices, Deck).

swap_cards(Hand, [], Deck) ->
    {Hand, Deck};
swap_cards(Hand, Indices, Deck) ->
    [First | Rest] = Indices,
    {NewHand, Deck_after} = swap_card(Hand, First, Deck),
    swap_cards(NewHand, Rest, Deck_after).
