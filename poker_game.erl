-module(poker_game).

-export([reset_game/1]).

reset_game(Players) ->
    Deck = deck:shuffle_deck(),
    {Hands, Deck_after} = deal_hands([], Deck, Players),
    Hands.

deal_hands(Hands, Deck, 0) ->
    {Hands, Deck};    
deal_hands(Hands, Deck, Count) ->
    {Deck_after, Hand} = deck:get_hand(Deck),
    Dealt_hands = Hands ++ [Hand],
    deal_hands(Dealt_hands, Deck_after, Count-1).
