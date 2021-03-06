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

my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_element5(A,B):-member(B,A).
my_tolower6(A,B):-downcase_atom(A,B).
my_max_list7(A,B):-max_list(A,B).
my_msort8(A,B):-msort(A,B).
my_len9(A,B):-length(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_set12(A):-list_to_set(A,A).

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

my_uppercase14(A):-upcase_atom(A,A).
my_double15(N,M):-M is 2*N,M =< 10.
my_sumlist16(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
prim(my_succ4/2).
prim(my_element5/2).
prim(my_tolower6/2).
prim(my_max_list7/2).
prim(my_msort8/2).
prim(my_len9/2).
prim(my_toupper10/2).
prim(my_lowercase11/1).
prim(my_set12/1).
prim(my_uppercase14/1).
prim(my_double15/2).
prim(my_sumlist16/2).
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
p([['R','e','j'],['J','w','q']],[['R','e'],['J','w']]).
p([['Z','t','u'],['D','s','S'],['k','I','z'],['r','l','q']],[['Z','t'],['D','s'],['k','I'],['r','l']]).
p([['P','J','C'],['J','g','t','e'],['w','s','R'],['M','W','h']],[['P','J'],['J','g','t'],['w','s'],['M','W']]).
p([['C','g','w','p'],['A','e','l'],['f','Z','X'],['w','q','h','Y']],[['C','g','w'],['A','e'],['f','Z'],['w','q','h']]).
p([['H','S','p','U'],['A','Y','R','t'],['D','J','v']],[['H','S','p'],['A','Y','R'],['D','J']]).
q([['j','q','N'],['k','C','j'],['D','Z','f']],[['j','q','N'],['k','C'],['D','Z','f']]).
q([['D','g','U','M'],['g','p','U']],[['D','g','U','M'],['g','p']]).
q([['X','Y','D','X'],['r','N','h']],[['X','Y','D','X'],['r','N']]).
q([['q','B','A'],['W','m','I'],['N','H','E'],['l','m','f']],[['q','B','A'],['W','m','I'],['N','H'],['l','m']]).
q([['T','d','R'],['E','r','K'],['i','H','Q'],['Q','n','q']],[['T','d','R'],['E','r','K'],['i','H','Q'],['Q','n']]).
