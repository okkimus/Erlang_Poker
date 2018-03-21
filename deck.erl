-module(deck).

-export([get_hand/1, get_card/1, get_cards/2, shuffle_deck/0]).

get_hand(Deck) ->
    {Deck_after, Hand} = get_cards(5, Deck),
    {Deck_after, Hand}.

get_card(Deck) ->
    [Top_card | Deck_after] = Deck,
    {Top_card, Deck_after}.

get_cards(Count, Deck) ->
    {Deck_after, Hand} = loop_cards({Count, Deck, []}),
    {Deck_after, Hand}.

shuffle_deck() ->
    Deck = create_deck(),
    Random_list = [{random:uniform(), Card} || Card <- Deck],
    [Card || {_,Card} <- lists:sort(Random_list)].

create_deck() -> 
    Suits = [hearts, diamonds, clubs, spades],
    Values = [ace, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
    Cards = [{S, V} || S <- Suits, V <- Values],
    Cards.

loop_cards({Count, Deck, Cards}) -> loop_cards(Count, Deck, Cards).
    
loop_cards(0, Deck, Cards) ->
    {Deck, Cards};
loop_cards(Count, Deck, Cards) ->
    {Top_card, Deck_after} = get_card(Deck),
    New_cards = Cards ++ [Top_card],
    loop_cards(Count-1, Deck_after, New_cards).