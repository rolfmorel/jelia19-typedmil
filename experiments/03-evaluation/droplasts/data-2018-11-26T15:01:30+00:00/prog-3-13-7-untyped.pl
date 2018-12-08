:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_tolower3(A,B):-downcase_atom(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_set5(A):-list_to_set(A,A).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_head9([H|_],H).
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_last12(A,B):-last(A,B).
my_lowercase13(A):-downcase_atom(A,A).
my_msort14(A,B):-msort(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_tolower3/2).
prim(my_uppercase4/1).
prim(my_set5/1).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_head9/2).
prim(my_pred10/2).
prim(my_odd11/1).
prim(my_last12/2).
prim(my_lowercase13/1).
prim(my_msort14/2).
prim(my_succ15/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['H','q','T','l'],['I','G','o']],[['H','q','T'],['I','G']]).
p([['H','R','L','h'],['K','D','i','X'],['F','I','u'],['e','J','g']],[['H','R','L'],['K','D','i'],['F','I'],['e','J']]).
p([['d','x','G','o'],['a','K','W']],[['d','x','G'],['a','K']]).
p([['g','Z','L','t'],['f','F','Q'],['U','P','E'],['W','E','O','A']],[['g','Z','L'],['f','F'],['U','P'],['W','E','O']]).
p([['J','N','K'],['a','u','I']],[['J','N'],['a','u']]).
q([['X','E','N','Z'],['G','F','W','G'],['W','P','X','p']],[['X','E','N','Z'],['G','F','W','G'],['W','P','X']]).
q([['U','J','X'],['B','G','b','X']],[['U','J'],['B','G','b','X']]).
q([['r','h','I'],['x','P','d'],['S','e','U','h'],['k','a','O','a']],[['r','h'],['x','P'],['S','e','U','h'],['k','a','O','a']]).
q([['n','W','D','H'],['T','l','T'],['J','b','H']],[['n','W','D','H'],['T','l'],['J','b','H']]).
q([['r','p','A','D'],['k','c','R'],['i','X','S','U']],[['r','p','A','D'],['k','c','R'],['i','X','S']]).
