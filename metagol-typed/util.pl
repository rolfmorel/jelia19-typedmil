:- module(util,[dtype/2,gtype/2,strip_type/2,strip_types/2,add_type_vars/2,const_list/2,zip_goal_types/3]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

dtype(_:T:_,T).
gtype(_:_:T,T).

strip_type(A:_:_,A):-!.
strip_type(A:_,A):-!.

strip_types([],[]).
strip_types([A1|T1],[A2|T2]):-strip_type(A1,A2),strip_types(T1,T2).

add_type_vars(T,T:_:_).

const_list(_,[]).
const_list(C,[C|T]):-const_list(C,T).

zip_goal_types([],[],[]).
%zip_goal_types(['@'(A1)|L1],[_|L2],['@'(A1)|Rest]):-!,
%  zip_goal_types(L1,L2,Rest).
zip_goal_types([Atom:Type1|L1],[Atom:Type2|L2],[Atom:Type1:Type2|Rest]):-
  zip_goal_types(L1,L2,Rest).

