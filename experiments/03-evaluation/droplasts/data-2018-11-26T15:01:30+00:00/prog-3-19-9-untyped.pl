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

my_odd3(A):-1 is A mod 2.
my_sumlist4(A,B):-sumlist(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_set6(A):-list_to_set(A,A).
my_double7(N,M):-M is 2*N,M =< 10.
my_tolower8(A,B):-downcase_atom(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_flatten10(A,B):-flatten(A,B).
my_element11(A,B):-member(B,A).
my_msort12(A,B):-msort(A,B).
my_succ13(A,B):-succ(A,B),B =< 10.
my_last14(A,B):-last(A,B).
my_toupper15(A,B):-upcase_atom(A,B).

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

my_min_list17(A,B):-min_list(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_uppercase19(A):-upcase_atom(A,A).
my_max_list20(A,B):-max_list(A,B).
my_even21(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_odd3/1).
prim(my_sumlist4/2).
prim(my_lowercase5/1).
prim(my_set6/1).
prim(my_double7/2).
prim(my_tolower8/2).
prim(my_pred9/2).
prim(my_flatten10/2).
prim(my_element11/2).
prim(my_msort12/2).
prim(my_succ13/2).
prim(my_last14/2).
prim(my_toupper15/2).
prim(my_min_list17/2).
prim(my_list_to_set18/2).
prim(my_uppercase19/1).
prim(my_max_list20/2).
prim(my_even21/1).
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
p([['L','g','k'],['s','C','C','n'],['r','q','Q','V']],[['L','g'],['s','C','C'],['r','q','Q']]).
p([['Q','a','i'],['J','e','m'],['F','i','M']],[['Q','a'],['J','e'],['F','i']]).
p([['V','j','m','s'],['R','J','y'],['g','W','f','T']],[['V','j','m'],['R','J'],['g','W','f']]).
p([['K','i','n'],['N','N','I'],['e','w','J','s'],['A','j','D']],[['K','i'],['N','N'],['e','w','J'],['A','j']]).
p([['L','x','t'],['o','Z','P'],['p','p','d','h'],['Q','K','p']],[['L','x'],['o','Z'],['p','p','d'],['Q','K']]).
q([['G','g','l','d'],['D','S','u'],['s','t','z','d']],[['G','g','l','d'],['D','S'],['s','t','z','d']]).
q([['e','l','G','o'],['x','r','v','r']],[['e','l','G'],['x','r','v','r']]).
q([['B','i','O'],['p','i','d'],['o','a','I'],['U','K','h','p']],[['B','i','O'],['p','i'],['o','a','I'],['U','K','h']]).
q([['h','B','Y'],['a','n','t'],['g','M','U'],['I','c','m']],[['h','B'],['a','n','t'],['g','M','U'],['I','c']]).
q([['D','N','f','m'],['w','z','r'],['C','T','O','n']],[['D','N','f','m'],['w','z'],['C','T','O','n']]).
