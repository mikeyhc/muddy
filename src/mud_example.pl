:- use_module(library(muddy)).

:- multifile muddy:message_handler/1.

muddy:message_handler(privmsg(Nick, _, _, 'muddy: register')) :-
    send_to_sever(privmsg(Nick, 'registration not yet available')).
