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

my_succ3(A,B):-succ(A,B),B =< 10.
my_msort4(A,B):-msort(A,B).
my_set5(A):-list_to_set(A,A).
my_tolower6(A,B):-downcase_atom(A,B).
my_odd7(A):-1 is A mod 2.
my_toupper8(A,B):-upcase_atom(A,B).
my_uppercase9(A):-upcase_atom(A,A).
my_min_list10(A,B):-min_list(A,B).
my_element11(A,B):-member(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_len14(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_msort4/2).
prim(my_set5/1).
prim(my_tolower6/2).
prim(my_odd7/1).
prim(my_toupper8/2).
prim(my_uppercase9/1).
prim(my_min_list10/2).
prim(my_element11/2).
prim(my_sumlist12/2).
prim(my_pred13/2).
prim(my_len14/2).
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
p([['H','V','l','U'],['r','t','a'],['s','t','C']],[['H','V','l'],['r','t'],['s','t']]).
p([['T','a','H','X'],['k','S','Z','h'],['V','Y','N','T']],[['T','a','H'],['k','S','Z'],['V','Y','N']]).
p([['c','V','P'],['g','D','m','s'],['n','o','W','j'],['c','C','x','p']],[['c','V'],['g','D','m'],['n','o','W'],['c','C','x']]).
p([['j','Q','a'],['L','g','u','g'],['w','k','J'],['f','L','N']],[['j','Q'],['L','g','u'],['w','k'],['f','L']]).
p([['a','D','Y'],['W','Z','j','Z'],['B','C','i'],['w','Z','e','z']],[['a','D'],['W','Z','j'],['B','C'],['w','Z','e']]).
q([['N','D','O','k'],['H','I','r']],[['N','D','O'],['H','I','r']]).
q([['y','G','Z','O'],['J','N','h','O'],['R','R','U','i']],[['y','G','Z'],['J','N','h','O'],['R','R','U','i']]).
q([['u','y','w'],['X','p','H','l'],['F','w','j','X'],['a','N','I','F']],[['u','y','w'],['X','p','H','l'],['F','w','j','X'],['a','N','I']]).
q([['A','D','A'],['j','e','i','U'],['C','X','B'],['K','S','O']],[['A','D','A'],['j','e','i','U'],['C','X','B'],['K','S']]).
q([['S','D','H'],['G','d','M','X'],['s','V','Z','B']],[['S','D','H'],['G','d','M','X'],['s','V','Z']]).
