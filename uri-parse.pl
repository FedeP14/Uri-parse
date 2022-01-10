:- module(uri_parse, [uri_parse/2, uri_display/1]).

uri_parse(URIString, uri(Scheme, Userinfo, Host, Port, Path, Query, Frag)) :-
    string_chars(URIString, URIChars),
    phrase(helpUri(Scheme, Userinfo, Host, Port, Path, Query, Frag), URIChars).

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
helpUri(Scheme, Userinfo, Host, Port, Path, Query, Frag) -->
    scheme(Scheme),
    {controllo_scheme(Scheme)},
    [':'],
    authority(Userinfo, Host, Port),
    subdomain(Path, Query, Frag),
    !.

% helpUri schemi speciali - URI2
% mailto
helpUri(Scheme, Userinfo, Host, 80, [], [], []) -->
    scheme(Scheme),
    {Scheme = 'mailto'},
    [':'],
    specialUserinfo(Userinfo),
    mailHost(Host),
    !.

% news
helpUri(Scheme, [], Host, 80, [], [], [] ) -->
    scheme(Scheme),
    {Scheme = 'news'},
    [':'],
    specialHost(Host),
    !.

% tel & fax

helpUri(Scheme, Userinfo, [], 80, [], [], []) -->
    scheme(Scheme),
    schemeTelFax(Scheme),
    [':'],
    specialUserinfo(Userinfo),
    !.


% zos
helpUri(Scheme, Userinfo, Host, Port, Path, Query, Frag) -->
    scheme(Scheme),
    {Scheme = 'zos'},
    [':'],
    authority(Userinfo, Host, Port),
    ['/'],
    specialSubdomain(Path, Query, Frag),
    !.

checkSlash(['/']) --> ['/'].
checkSlash([]) --> [].

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
    helpHost(Hostchars),
    {atom_chars(Host, Hostchars)}.

% Help gestione host con identificatori
helpHost(Host) -->
    identificatoreHost(A),
    [.],
    helpHost(B),
    {Hostlist = [A, ., B], flatten(Hostlist, Host)}.

helpHost(Host) --> identificatoreHost(Host).

% Help gestione host come Ip
ip(IP) -->
    threedigit(First),
    [.], 
    threedigit(Second),
    [.], 
    threedigit(Third),
    [.], 
    threedigit(Fourth), 
    {Iplist = [First, ., Second, ., Third, ., Fourth], 
        flatten(Iplist, Tempip), atom_chars(IP,Tempip)}.

threedigit(Digits) -->
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
    helpPort(Portchars),
    {atom_chars(Port, Portchars)}.

port(Port) --> {Portchars = ['8','0'], atom_chars(Port, Portchars)}.

% Help Gestione port
helpPort([First | Second]) -->
    [First],
    {is_digit(First)}, helpPort(Second).

helpPort([Port | []]) -->
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

specialSubdomain(Path, Query, Fragment) -->
    zosPath(Path),
    query(Query),
    fragment(Fragment),
    !.

specialHost(Host) -->
    host(Host).

specialHost([]) --> [].


% Gestione path
path(Path) -->
    helpPath(Pathchars), 
    {atom_chars(Path, Pathchars)}.



path([]) --> [].

% Help gestione path
helpPath(Path) --> 
    identificatorePath(A),
    ['/'],
    helpPath(B),
    {Pathlist = [A, /, B], flatten(Pathlist, Path)}.

helpPath(Path) --> identificatorePath(Path).


% Gestione query
query(Query) --> 
    ['?'],
    identificatoreQuery(Querylist),
    {atom_chars(Query, Querylist)}.

query([]) --> [].

% Gestione fragment
fragment(Fragment) --> 
    ['#'], 
    identificatoreFragment(Fragmentchars), 
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

identificatorePath(['%','2','0' | Tail]) -->
    [' '],
    identificatorePath(Tail),
    !.

% Identificatore Query
identificatoreQuery([Head | Tail]) --> 
    [Head], 
    {controllo_query(Head)}, 
    identificatoreQuery(Tail), 
    !.

identificatoreQuery([Head | []]) --> 
    [Head], 
    {controllo_query(Head)}.

identificatoreQuery(['%','2','0' | Tail]) -->
    [' '],
    identificatoreQuery(Tail),
    !.


% Caratteri non ammessi per query
controllo_query(Char) :-
    char_type(Char, graph),
    Char \= '#'.

% Identificatore fragment
identificatoreFragment([Head | Tail]) --> 
    [Head],
    {char_type(Head, alpha)},
    identificatoreFragment(Tail),
    !.

identificatoreFragment([Head | []]) --> 
    [Head], 
    {char_type(Head, alpha)}.

identificatoreFragment(['%','2','0' | Tail]) -->
    [' '],
    identificatoreFragment(Tail),
    !.

% Schema mailto

specialUserinfo(Userinfo) -->
    identificatore(Userinfochars),
    {atom_chars(Userinfo, Userinfochars)}.

specialUserinfo([]) --> [].

mailIdentificatore(Userinfochars) --> 
   identificatore(Userinfochars).

mailHost(Host) -->
    ['@'],
    host(Host).

mailHost([]) --> [].

% tel and fax
schemeTelFax(Scheme) --> {Scheme = 'tel'}.

schemeTelFax(Scheme) --> {Scheme = 'fax'}.

% zosPath

zosPath(Path) --> 
    id44(Path44),
    ['('],
    id8(Path8),
    [')'],
    {atom_length(Path44, Path44maxlength), Path44maxlength < 45},
    {atom_length(Path8, Path8maxlength), Path8maxlength < 9},
    {TempPath = [Path44, '(', Path8, ')'], 
        flatten(TempPath, PathList), 
        atom_chars(Path, PathList)}.

zosPath(Path) --> 
    id44(PathList),
    {atom_chars(Path, PathList)}.

% id44
id44([Head | Tail]) -->
    [Head],
    {char_type(Head, alpha)},
    bodyid44(Tail),
    !.

bodyid44([Head | Tail]) -->
    [Head],
    controllo_zos(Head),
    bodyid44(Tail),
    !.

bodyid44([Head | []]) -->
    [Head],
    {char_type(Head, alnum)}.

%id8
id8([Head | Tail]) -->
    [Head],
    {char_type(Head, alpha)},
    bodyid8(Tail),
    !.

bodyid8([Head | Tail]) -->
    [Head],
    {char_type(Head, alnum)},
    bodyid8(Tail).

bodyid8([Head | []]) -->
    [Head],
    {char_type(Head, alnum)}.

% controllo zos
controllo_zos(Head) --> {char_type(Head, alnum)}.

controllo_zos(Head) --> {Head = '.'}.
