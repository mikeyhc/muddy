:- module(muddy, [ get_outstream/1,
                   get_instream/1,
                   construct_server_message//1,
                   server_msg_handler//1,
                   message_handler/1,
                   connect_helper/1,
                   connect_helper/2,
                   connect/1,
                   connect/2,
                   send_to_server/1,
                   enable_debug/0,
                   disable_debug/0
                 ]).

:- use_module(library(socket)).

:- dynamic server_streams/2, auth_count/1, muddy_thread/1, start_time/1.
:- dynamic debug/1.

:- multifile construct_server_message//1, server_msg_handler//1.
:- multifile message_handler/1.

enable_debug :-
    retract(debug(false)),!,
    asserta(debug(true)).

disable_debug :-
    retract(debug(true)),!,
    asserta(debug(false)).

debug_message_(Format, Args) :-
    debug(true),
    atom_concat('debug: ', Format, FmtStr),
    format(FmtStr, Args), !.
debug_message_(_, _).

debug_message(Format, Args) :-
    atom_concat(Format, '~n', NFormat),
    debug_message_(NFormat, Args).

connect(Server) :- connect(Server, 6667).
connect(Server, Port) :-
    thread_create(connect_helper(Server, Port), ThreadID, []),
    asserta(muddy_thread(ThreadID)).

disconnect :-
    muddy_thread(TID),
    retract(muddy_thread(TID)),
    thread_signal(TID, throw(disconnect_exception)).

connect_helper(Server) :- connect_helper(Server, 6667).
connect_helper(Server, Port) :-
    tcp_socket(Socket),
    init_loop,
    catch(handle_connection(Socket, Server, Port),
          Exception,
          format('exception: ~w~n', [Exception])),
    cleanup_connection(Socket).  
init_loop :- init_bot, fail.
init_loop.

init_bot :-
    retractall(auth_count(_)),
    asserta(auth_count(0)).

handle_connection(Socket, Server, Port) :-
    tcp_connect(Socket, Server:Port),
    debug_message('Connecting to ~w:~w', [Server, Port]),
    tcp_open_socket(Socket, InStream, OutStream),
    set_stream(OutStream, buffer(line)),
    retractall(server_streams(_, _)),
    asserta(server_streams(InStream, OutStream)),
    debug_message('Connected', []),
    get_time(T),
    floor(T, X),
    asserta(start_time(X)),
    asserta(debug(true)),
    chat_to_server.

cleanup_connection(Socket) :-
    get_outstream(OutStream),
    format(OutStream, "QUIT~n", []),
    retractall(server_streams(_, _)),
    retractall(start_time(_)),
    tcp_close_socket(Socket).

get_instream(InStream) :- server_streams(InStream, _).
get_outstream(OutStream) :- server_streams(_, OutStream).

chat_to_server :-
    get_line(Line),
    debug_message('got line ~s', [Line]),
    catch((parse_server_msg(Line, Msg),
           handle_message(Msg)),
          Except,
          handle_error(Except)),!,
    chat_to_server.

get_line(Line) :-
    get_instream(InStream),
    get_char(InStream, Char),
    eat_nl(InStream, Char, NewChar),
    get_line(InStream, NewChar, InLine),
    maplist(atom_codes, InLine, TempList),
    flatten(TempList, Line).

get_line(_, end_of_file, []) :- throw(exception(end_of_stream)).
get_line(_, '\n', []) :- !.
get_line(_, '\r', []) :- !.
get_line(Stream, H, [H|T]) :- get_char(Stream, C), get_line(Stream, C, T).

eat_nl(_, end_of_file, _) :- throw(exception(end_of_stream)).
eat_nl(Stream, C, R) :-
    (C = '\n'; C = '\r'),
    get_char(Stream, L), !,
    eat_nl(Stream, L, R).
eat_nl(_, R, R).

handle_message(Msg) :- message_handler(Msg).
handle_message(Msg) :- throw(no_message_handler(Msg)).

handle_error(Error) :- debug_message('error: ~w', [Error]).

inc_auth_count :-
    auth_count(A),
    retractall(auth_count(_)),
    B is A + 1,
    asserta(auth_count(B)).

message_handler(ping(Reply)) :- send_to_server(pong(Reply)).
message_handler(notice(_, 'AUTH', _)) :-
    auth_count(3),
    register_nick,
    register_user,
    inc_auth_count.
message_handler(notice(_, 'AUTH', _)) :- inc_auth_count.
message_handler(mode(_)) :- join_bots.
message_handler(privmsg(_, _, Chan, 'muddy: uptime')) :-
    get_time(PCT),
    floor(PCT, CT),
    start_time(ST),
    Diff is CT - ST,
    Sec is Diff mod 60,
    Min is Diff // 60 mod 60,
    Hour is Diff // 3600 mod 24,
    Day is Diff // 84600,
    format(atom(String), "~dd ~dh ~dm ~ds~n",
           [ Day, Hour, Min, Sec ]),
    send_to_server(privmsg(Chan, String)).

    
register_nick :- send_to_server(nick(muddy)).
register_user :- send_to_server(user(muddy, 'Muddy Bot')).
join_bots :- send_to_server(join('#bots')).

send_to_server(Msg) :- 
    get_outstream(OutStream),
    server_message(Msg, SMsg),
    debug_message_('sending ~w', SMsg),
    format(OutStream, "~w~n", SMsg).

parse_server_msg(Line, Msg) :- parse_server_msg(Msg, Line, []), !.
parse_server_msg(SLine, _) :- 
    atom_codes(Line, SLine),
    throw(unhandled_irc_message(Line)).
parse_server_msg(Msg) --> server_msg_handler(Msg), !.

server_message(Msg, SMsg) :- server_message(Msg, S, []),
                             atom_codes(SMsg, S).

server_message(Msg) --> construct_server_message(Msg), !.
server_message(Msg) --> { throw(unknown_message(Msg)) }.

construct_server_message(pong(Reply)) --> 
    { atom_codes(Reply, SReply) }, 
    "PONG :", SReply, "\n".
construct_server_message(nick(Nick)) -->
    { atom_codes(Nick, SNick) },
    "NICK ",  SNick, "\n".
construct_server_message(user(X, Y)) -->
    { atom_codes(X, SX), atom_codes(Y, SY) },
    "USER ", SX, " 8 * : ", SY, "\n".
construct_server_message(join(Chan)) -->
    { atom_codes(Chan, SChan) },
    "JOIN ", SChan, "\n".
construct_server_message(privmsg(Target, Message)) -->
    { atom_codes(Target, STarget), atom_codes(Message, SMessage) },
    "PRIVMSG ", STarget, " :", SMessage, "\n".

server_msg_handler(ping(Reply)) -->
    "PING :", not_nl(SReply), eat_nl, { atom_codes(Reply, SReply) }.
server_msg_handler(notice(Server, 'AUTH', Msg)) -->
    ":", not_space(SServer), " NOTICE AUTH :*** ", not_nl(SMsg),
    { atom_codes(Server, SServer), atom_codes(Msg, SMsg) }.
server_msg_handler(mode(Mode)) -->
    ":muddy MODE muddy :", not_nl(SMode), 
    { atom_codes(Mode, SMode) }.
server_msg_handler(privmsg(Nick, User, Chan, Msg)) -->
    { atom_codes('!', [ExclaimCode|_]) },
    ":", not_char(ExclaimCode, SNick), "!", not_space(SUser), " PRIVMSG ", 
    not_space(SChan), " :", not_nl(SMsg),
    { atom_codes(Nick, SNick), atom_codes(User, SUser), 
      atom_codes(Chan, SChan), atom_codes(Msg, SMsg) }.
    
not_char(Y, [X|T]) --> [X], { X \= Y }, !, not_char(Y, T).
not_char(_, []) --> [].

not_space([X|Y]) --> [X], { \+ is_whitespace(X) }, !, not_space(Y).
not_space([]) --> [].
not_nl([X|Y]) --> [X], { \+ is_newline(X) }, !, not_nl(Y).
not_nl([]) --> [].

eat_nl --> [X], { \+ is_newline(X) }, !, eat_nl.
eat_nl --> [].

is_whitespace(32).
is_whitespace(9).
is_whitespace(X) :- is_newline(X).

is_newline(10).
is_newline(13).
