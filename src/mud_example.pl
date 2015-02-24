:- use_module(library(muddy)).

:- multifile muddy:message_handler/1.
:- dynamic user/2.

class_list([ warrior,
             rogue,
             mage
           ]).

class_base(warrior, 5, 1, 1, 5).
class_base(rogue, 3, 5, 1, 3).
class_base(mage, 1, 3, 5, 3).

muddy:message_handler(privmsg(Nick, User, muddy, Msg)) :-
    atom_concat('muddy: ', Tail, Msg),
    handle_privmsg(Nick, User, Tail).
muddy:message_handler(privmsg(Nick, User, muddy, Msg)) :-
    handle_privmsg(Nick, User, Msg).
muddy:message_handler(privmsg(Nick, User, _, Msg)) :-
    atom_concat('muddy: ', Tail, Msg),
    handle_privmsg(Nick, User, Tail).

send_to_server(Chan, Msg) :- send_to_server(privmsg(Chan, Msg)).

send_list_elem(Chan, Msg) :-
    atom_concat('    ', Msg, Str),
    send_to_server(Chan, Str).

handle_privmsg(Nick, User, register) :-
    user(User, _), !,
    send_to_server(Nick, 'already registered').
handle_privmsg(Nick, User, register) :-
    assert(user(User, false)),
    send_to_server(Nick, 'registered, now select a class').

handle_privmsg(Nick, User, Msg) :-
    atom_concat('select class ', _, Msg),
    user(User, X), X \= false, !,
    send_to_server(Nick, 'class already selected').
handle_privmsg(Nick, User, Msg) :-
    atom_concat('select class ', Class, Msg),
    class_list(ClassList),
    member(Class, ClassList),
    retract(user(User, false)),
    assert(user(User, Class)),
    send_to_server(Nick, 'class selected').

handle_privmsg(Nick, _, classlist) :-
    class_list(ClassList),
    send_to_server(Nick, 'classlist:'),
    maplist(send_list_elem(Nick),  ClassList).

handle_privmsg(Nick, _, help) :-
    send_to_server(Nick, 'available commands: '),
    maplist(send_list_elem(Nick), [ help,
                                    register,
                                    classlist,
                                    'select class <class>'
                                  ]).

handle_privmsg(Nick, User, me) :-
    user(User, false), !,
    send_to_server(Nick, 'you have not selected a class').
handle_privmsg(Nick, User, me) :-
    user(User, Class),
    class_base(Class, Str, Dex, Int, Con),
    atom_concat('You are a ', Class, ClassMsg),
    atom_concat('Str: ', Str, StrMsg),
    atom_concat('Dex: ', Dex, DexMsg),
    atom_concat('Int: ', Int, IntMsg),
    atom_concat('Con: ', Con, ConMsg),
    send_to_server(Nick, ClassMsg),
    maplist(send_list_elem(Nick), [ StrMsg,
                                    DexMsg,
                                    IntMsg,
                                    ConMsg ]).

handle_privmsg(Nick, User, _) :-
    user(User, _), !,
    send_to_server(Nick, 'not registered').

handle_privmsg(Nick, _, Msg) :-
    atom_concat('Unknown message: ', Msg, Str),
    send_to_server(Nick, Str).
