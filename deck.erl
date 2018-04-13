%%% Deck module takes care of creating deck, dealing hands and cards.
-module(deck).

-export([get_hand/1, get_cards/2, shuffle_deck/0]).

%%----------------------------------------------------------------------
%% Function: get_hand/1
%% Purpose: Get hand (5 cards) from given deck.
%% Args: Deck of cards you want hand dealed from.
%% Returns: Tuple with a list of cards {Suit, Value} and Deck after
%%          dealing hand.
%%----------------------------------------------------------------------
get_hand(Deck) ->
    {Deck_after, Hand} = get_cards(5, Deck),
    {Deck_after, Hand}.

%%----------------------------------------------------------------------
%% Function: get_card/1
%% Purpose: Get one card from given deck.
%% Args: Deck of cards you want a card dealed from.
%% Returns: Tuple with drawn card {Suit, Value} and Deck after
%%          dealing hand.
%%----------------------------------------------------------------------
get_card(Deck) ->
    [Top_card | Deck_after] = Deck,
    {Top_card, Deck_after}.

%%----------------------------------------------------------------------
%% Function: get_cards/2
%% Purpose: Get cards from given deck.
%% Args: Count of how many cards you want. Deck of cards you want the 
%%       cards dealed from.
%% Returns: Tuple with deck after dealing the cards and the drawn cards.
%%----------------------------------------------------------------------
get_cards(Count, Deck) ->
    {Deck_after, Cards} = loop_cards({Count, Deck, []}),
    {Deck_after, Cards}.

%%----------------------------------------------------------------------
%% Function: shuffle_deck/0
%% Purpose: Creates a deck of playing cards and shuffles it.
%% Args: -
%% Returns: List of cards {Suit, Value} in randomized order.
%%----------------------------------------------------------------------
shuffle_deck() ->
    Deck = create_deck(),
    % Shuffles the deck by creating list of tuples with random numbers
    % and the cards.
    Random_list = [{random:uniform(), Card} || Card <- Deck],
    % Sorting the list by the random numbers -> Deck gets shuffled.
    [Card || {_,Card} <- lists:sort(Random_list)].

%%----------------------------------------------------------------------
%% Function: create_deck/0
%% Purpose: Creates a deck of playing cards.
%% Args: -
%% Returns: List of cards {Suit, Value}.
%%----------------------------------------------------------------------
create_deck() -> 
    Suits = [hearts, diamonds, clubs, spades],
    Values = [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
    Cards = [{S, V} || S <- Suits, V <- Values],
    Cards.

%%----------------------------------------------------------------------
%% Function: loops_cards/1
%% Purpose: Helper function for getting bunch of cards. Calls the same
%%          funtion inside the module with more complex syntax.
%% Args: Tuple of number of cards wanted and deck you want those cards
%%       from.
%% Returns: Deck after drawing cards and list of cards drawn
%%          {Suit, Value}.
%%----------------------------------------------------------------------
loop_cards({Count, Deck}) -> loop_cards(Count, Deck, []).
    
%%----------------------------------------------------------------------
%% Function: loops_cards/3
%% Purpose: Get bunch of cards from deck.
%% Args: Count of cards wanted, deck where the cards are drawn and
%%       list of cards already drawn.
%% Returns: Deck after drawing cards and list of cards drawn
%%          {Suit, Value}.
%%----------------------------------------------------------------------
loop_cards(0, Deck, Cards) ->
    {Deck, Cards};
loop_cards(Count, Deck, Cards) ->
    {Top_card, Deck_after} = get_card(Deck),
    New_cards = Cards ++ [Top_card],
    loop_cards(Count-1, Deck_after, New_cards).