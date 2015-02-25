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

dispatch(Nick, User, Msg) :-
    dispatch_list(DL),
    dispatch_option(Msg, DL, Option),
    check_options(Nick, User, Options),
    call(Option, Nick, User, Msg).

check_options(_, _, option(_, _)).
check_options(Nick, User, option(_, _, OptList)) :- 
    check_opt_list(Nick, User, OptList).
check_options(Nick, User, option(_, _, OptList, _)) :-
    check_opt_list(Nick, User, OptList).

check_opt_list(_, _, []).
check_opt_list(Nick, User, [H|T]) :- 
    check_option_value(Nick, User, H), !,
    check_opt_list(T).

% not registered
check_option_value(_, User, not_registered) :- \+ user(User, _), !.
check_option_value(Nick, _, not_registered) :-
    send_to_server(Nick, 'you must not be registered to do that'), !.

% registered
check_option_value(_, User, registered) :- user(User, _), !.
check_option_value(Nick, _, registered) :- 
    send_to_server(Nick, 'you must be registered to do that'), !.

% no class
check_option_value(_, User, no_class) :- user(User, C), C = false, !.
check_option_value(_, User, no_class) :- \+ user(User, _), !,
    send_to_server(Nick, 'you must be registered to do that').
check_option_value(Nick, _, no_class) :-
    send_to_server(Nick, 'you must not have a class to do that').

% class
check_option_value(_, User, class) :- user(User, C), C \= false, !.
check_option_value(Nick, _, class) :- \+ user(User, _), !,
    send_to_server(Nick, 'you must be registered to do that').
check_option_value(Nick, _, class) :-
    send_to_server(Nick, 'you must have a class to do that').

dispatch_list([ option(register, register, [ not_registered ]),
                option(prefix('select class '), select_class, 
                       [ no_class, registered ], 'select class <class>'),
                option(classlist, classslist),
                option(help, help),
                option(me, me, [ class ])
              ]).


handle_privmsg(Nick, User, register) :-
    user(User, _), !,
    send_to_server(Nick, 'already registered').
handle_privmsg(Nick, User, register) :-
    assert(user(User, false)),
    send_to_server(Nick, 'registered, now select a class').

handle_privmsg(Nick, User, Msg) :-
    atom_concat('select class ', _, Msg),
    \+ user(User, _), !,
    send_to_server(Nick, 'you have not registered').
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
                                    'me {RC}',
                                    'select class <class> {R}',
                                    '',
                                    '{C} = have selected a class',
                                    '{R} = have registered'
                                  ]).

handle_privmsg(Nick, User, me) :-
    \+ user(User, _), !,
    send_to_server(Nick, 'you have not registered').
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
