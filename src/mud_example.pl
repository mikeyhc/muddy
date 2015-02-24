:- use_module(library(muddy)).

:- multifile muddy:message_handler/1.
:- dynamic user/2.

class_list([ warrior,
             rogue,
             mage
           ]).

muddy:message_handler(privmsg(Nick, User, muddy, Msg)) :-
    atom_concat('muddy: ', Tail, Msg),
    handle_privmsg(Nick, User, Tail).
muddy:message_handler(privmsg(Nick, User, muddy, Msg)) :-
    handle_privmsg(Nick, User, Msg).
muddy:message_handler(privmsg(Nick, User, _, Msg)) :-
    atom_concat('muddy: ', Tail, Msg),
    handle_privmsg(Nick, User, Tail).

send_list_elem(Chan, Msg) :-
    atom_concat('    ', Msg, Str),
    send_to_server(privmsg(Chan, Str)).

handle_privmsg(Nick, User, register) :-
    user(User, _), !,
    send_to_server(privmsg(Nick, 'already registered')).
handle_privmsg(Nick, User, register) :-
    assert(user(User, false)),
    send_to_server(privmsg(Nick, 'registered, now select a class')).

handle_privmsg(Nick, User, Msg) :-
    atom_concat('select class ', _, Msg),
    \+ user(User, false), !,
    send_to_server(privmsg(Nick, 'class already selected')).
handle_privmsg(Nick, User, Msg) :-
    atom_concat('select class ', Class, Msg),
    class_list(ClassList),
    member(Class, ClassList),
    retract(user(User, false)),
    assert(user(User, Class)),
    send_to_server(privmsg(Nick, 'class selected')).

handle_privmsg(Nick, _, classlist) :-
    class_list(ClassList),
    send_to_server(privmsg(Nick, 'classlist:')),
    maplist(send_list_elem(Nick),  ClassList).

handle_privmsg(Nick, _, help) :-
    send_to_server(privmsg(Nick, 'available commands: ')),
    maplist(send_list_elem(Nick), [ help,
                                    register,
                                    classlist,
                                    'select class <class>'
                                  ]).
