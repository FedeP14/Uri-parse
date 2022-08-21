%%%% -- Mode: Prolog --
% 872491 - Pulcino Federico

:- module(uri_parse, [uri_parse/2, uri_display/1, uri_display/2]).

uri_parse(URIString, uri(Scheme, Userinfo, Host, Port, Path, Query, Frag)) :-
    string_chars(URIString, URIChars),
    phrase(help_uri(Scheme, Userinfo, Host, Port, Path, Query, Frag), URIChars).

uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Frag)) :-
    format(
    'Scheme: ~w~n\c
    Userinfo: ~w~n\c
    Host: ~w~n\c
    Port: ~w~n\c
    Path: ~w~n\c
    Query: ~w~n\c 
    Fragment: ~w~n',
    [Scheme, Userinfo, Host, Port, Path, Query, Frag]).

    
uri_display(URI, File) :-
    tell(File),
    uri_parse(URI, PURI),
    write(PURI),
    told().


% URI1
help_uri(Scheme, Userinfo, Host, Port, Path, Query, Frag) -->
    scheme(Scheme),
    {controllo_scheme(Scheme)},
    [':'],
    authority(Userinfo, Host, Port),
    subdomain(Path, Query, Frag),
    !.

% help_uri schemi speciali - URI2
% mailto
help_uri(Scheme, Userinfo, Host, 80, [], [], []) -->
    scheme(Scheme),
    {Scheme = 'mailto'},
    [':'],
    special_userinfo(Userinfo),
    mail_host(Host),
    !.

% news
help_uri(Scheme, [], Host, 80, [], [], [] ) -->
    scheme(Scheme),
    {Scheme = 'news'},
    [':'],
    special_host(Host),
    !.

% tel & fax

help_uri(Scheme, Userinfo, [], 80, [], [], []) -->
    scheme(Scheme),
    scheme_tel_fax(Scheme),
    [':'],
    special_userinfo(Userinfo),
    !.


% zos
help_uri(Scheme, Userinfo, Host, Port, Path, Query, Frag) -->
    scheme(Scheme),
    {Scheme = 'zos'},
    [':'],
    authority(Userinfo, Host, Port),
    ['/'],
    special_subdomain(Path, Query, Frag),
    !.

check_slash(['/']) --> ['/'].
check_slash([]) --> [].

% Gestione scheme
scheme(Scheme) -->
    identificatore(Schemechars), {atom_chars(Scheme, Schemechars)}.

% Controllo scheme
controllo_scheme(Scheme) :-
    Scheme \= 'mailto',
    Scheme \= 'news',
    Scheme \= 'tel',
    Scheme \= 'fax',
    Scheme \= 'zos'.

% Gestione authority
authority(UI, Host, Port) -->
    ['/'],['/'],
    userinfo(UI),
    host(Host),
    port(Port),
    !.

authority([], [], 80) --> [].


% Gestione userinfo
userinfo(UI) -->
    identificatore(UIchars),
    ['@'],
    {atom_chars(UI, UIchars)}.

userinfo([]) --> [].

% Gestione host come Ip
host(Host) --> ip(Host).

% Gestione host con identificatori
host(Host) -->
    help_host(Hostchars),
    {atom_chars(Host, Hostchars)}.

% Help gestione host con identificatori
help_host(Host) -->
    identificatoreHost(A),
    [.],
    help_host(B),
    {Hostlist = [A, ., B], flatten(Hostlist, Host)}.

help_host(Host) --> identificatoreHost(Host).

% Help gestione host come Ip
ip(IP) -->
    three_digits(First),
    [.], 
    three_digits(Second),
    [.], 
    three_digits(Third),
    [.], 
    three_digits(Fourth), 
    {Iplist = [First, ., Second, ., Third, ., Fourth], 
        flatten(Iplist, Tempip), atom_chars(IP,Tempip)}.

three_digits(Digits) -->
    numero(First), 
    numero(Second), 
    numero(Third), 
    {Digits = [First, Second, Third], 
        number_string(Number, Digits)},
    controllo_ip(Number).

controllo_ip(Number) --> {Number >= 0, Number < 256}.

numero(Digit) -->
    [Digit],
    {is_digit(Digit)}.

% Gestione port
port(Port) -->
    [:],
    help_port(Portchars),
    {atom_chars(Port, Portchars)}.

port(Port) --> {Portchars = ['8','0'], atom_chars(Port, Portchars)}.

% Help Gestione port
help_port([First | Second]) -->
    [First],
    {is_digit(First)}, help_port(Second).

help_port([Port | []]) -->
    [Port], 
    {is_digit(Port)}.

% Gestione subdomain

subdomain(Path, Query, Fragment) -->
    ['/'],
    path(Path),
    query(Query),
    fragment(Fragment),
    !.

subdomain([], [], []) --> [].

special_subdomain(Path, Query, Fragment) -->
    zos_path(Path),
    query(Query),
    fragment(Fragment),
    !.

special_host(Host) -->
    host(Host).

special_host([]) --> [].


% Gestione path
path(Path) -->
    help_path(Pathchars), 
    {atom_chars(Path, Pathchars)}.



path([]) --> [].

% Help gestione path
help_path(Path) --> 
    identificatorePath(A),
    ['/'],
    help_path(B),
    {Pathlist = [A, /, B], flatten(Pathlist, Path)}.

help_path(Path) --> identificatorePath(Path).


% Gestione query
query(Query) --> 
    ['?'],
    identificatore_query(Querylist),
    {atom_chars(Query, Querylist)}.

query([]) --> [].

% Gestione fragment
fragment(Fragment) --> 
    ['#'], 
    identificatore_fragment(Fragmentchars), 
    {atom_chars(Fragment, Fragmentchars)}.

fragment([]) --> [].


% Identificatore
identificatore([Head | Tail]) --> 
    [Head], 
    {controllo_char(Head)}, 
    identificatore(Tail), 
    !.

identificatore([Head | []]) --> 
    [Head], 
    {controllo_char(Head)}.

% Caratteri non ammessi per identificatore
controllo_char(Char) :- 
    char_type(Char, graph),
    Char \= '/',
    Char \= '?',
    Char \= '#',
    Char \= '@',
    Char \= ':'.

% identificatore host
identificatoreHost([Head | Tail]) --> 
    [Head], 
    {controllo_host(Head)}, 
    identificatoreHost(Tail), 
    !.

identificatoreHost([Head | []]) --> 
    [Head], 
    {controllo_host(Head)}.

% Caratteri non ammessi per identificatore-host
controllo_host(Char) :-
    char_type(Char, graph),
    Char \= '/',
    Char \= '?',
    Char \= '#',
    Char \= '@',
    Char \= ':',
    Char \= '.'.

% Identificatore Path
identificatorePath([Head | Tail]) --> 
    [Head],
    {controllo_char(Head), char_type(Head, graph)}, 
    identificatorePath(Tail), 
    !.

identificatorePath([Head | []]) --> 
    [Head], 
    {controllo_char(Head), char_type(Head, graph)}.

% Identificatore Query
identificatore_query([Head | Tail]) --> 
    [Head], 
    {controllo_query(Head)}, 
    identificatore_query(Tail), 
    !.

identificatore_query([Head | []]) --> 
    [Head], 
    {controllo_query(Head)}.

% Caratteri non ammessi per query
controllo_query(Char) :-
    char_type(Char, graph),
    Char \= '#'.

% Identificatore fragment
identificatore_fragment([Head | Tail]) --> 
    [Head],
    {char_type(Head, alpha)},
    identificatore_fragment(Tail),
    !.

identificatore_fragment([Head | []]) --> 
    [Head], 
    {char_type(Head, alpha)}.

% Schema mailto
special_userinfo(Userinfo) -->
    identificatore(Userinfochars),
    {atom_chars(Userinfo, Userinfochars)}.

special_userinfo([]) --> [].

identificatore_mail(Userinfochars) --> 
   identificatore(Userinfochars).

mail_host(Host) -->
    ['@'],
    host(Host).

mail_host([]) --> [].

% tel and fax
scheme_tel_fax(Scheme) --> {Scheme = 'tel'}.

scheme_tel_fax(Scheme) --> {Scheme = 'fax'}.

% zos_path
zos_path(Path) --> 
    id44(Path44),
    ['('],
    id8(Path8),
    [')'],
    {atom_length(Path44, Path44maxlength), Path44maxlength < 45},
    {atom_length(Path8, Path8maxlength), Path8maxlength < 9},
    {TempPath = [Path44, '(', Path8, ')'], 
        flatten(TempPath, PathList), 
        atom_chars(Path, PathList)}.

zos_path(Path) --> 
    id44(PathList),
    {atom_chars(Path, PathList)}.

% id44
id44([Head | Tail]) -->
    [Head],
    {char_type(Head, alpha)},
    body_id44(Tail),
    !.

body_id44([Head | Tail]) -->
    [Head],
    controllo_zos(Head),
    body_id44(Tail),
    !.

body_id44([Head | []]) -->
    [Head],
    {char_type(Head, alnum)}.

%id8
id8([Head | Tail]) -->
    [Head],
    {char_type(Head, alpha)},
    body_id8(Tail),
    !.

body_id8([Head | Tail]) -->
    [Head],
    {char_type(Head, alnum)},
    body_id8(Tail).

body_id8([Head | []]) -->
    [Head],
    {char_type(Head, alnum)}.

% controllo zos
controllo_zos(Head) --> {char_type(Head, alnum)}.

controllo_zos(Head) --> {Head = '.'}.