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

my_len3(A,B):-length(A,B).
my_lowercase4(A):-downcase_atom(A,A).
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

my_element7(A,B):-member(B,A).
my_last8(A,B):-last(A,B).
my_flatten9(A,B):-flatten(A,B).
my_odd10(A):-1 is A mod 2.
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_uppercase13(A):-upcase_atom(A,A).
my_double14(N,M):-M is 2*N,M =< 10.
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B),B =< 10.
my_list_to_set17(A,B):-list_to_set(A,B).
my_even18(A):-0 is A mod 2.
my_max_list19(A,B):-max_list(A,B).
my_toupper20(A,B):-upcase_atom(A,B).
my_msort21(A,B):-msort(A,B).
my_tolower22(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_len3/2).
prim(my_lowercase4/1).
prim(my_set5/1).
prim(my_element7/2).
prim(my_last8/2).
prim(my_flatten9/2).
prim(my_odd10/1).
prim(my_min_list11/2).
prim(my_pred12/2).
prim(my_uppercase13/1).
prim(my_double14/2).
prim(my_head15/2).
prim(my_succ16/2).
prim(my_list_to_set17/2).
prim(my_even18/1).
prim(my_max_list19/2).
prim(my_toupper20/2).
prim(my_msort21/2).
prim(my_tolower22/2).
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
p([['w','b','P'],['g','m','s','G'],['p','O','I','p']],[['w','b'],['g','m','s'],['p','O','I']]).
p([['V','m','s','O'],['e','S','C'],['u','p','I','w'],['i','Q','Q','f']],[['V','m','s'],['e','S'],['u','p','I'],['i','Q','Q']]).
p([['D','Z','Y'],['P','w','B']],[['D','Z'],['P','w']]).
p([['n','b','H'],['E','S','L'],['t','I','c']],[['n','b'],['E','S'],['t','I']]).
p([['v','F','u','t'],['b','p','I'],['t','m','N','s']],[['v','F','u'],['b','p'],['t','m','N']]).
q([['i','f','D'],['p','i','Z'],['s','W','R']],[['i','f'],['p','i','Z'],['s','W','R']]).
q([['p','g','d'],['v','d','V'],['i','C','O','e'],['p','C','Y','k']],[['p','g','d'],['v','d','V'],['i','C','O'],['p','C','Y','k']]).
q([['Y','a','E','p'],['f','G','o']],[['Y','a','E','p'],['f','G']]).
q([['T','s','n','M'],['J','U','C','C'],['o','g','M'],['D','I','O']],[['T','s','n','M'],['J','U','C','C'],['o','g'],['D','I']]).
q([['D','b','l','B'],['r','I','U'],['p','V','E']],[['D','b','l'],['r','I','U'],['p','V','E']]).
