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

my_set3(A):-list_to_set(A,A).
my_even4(A):-0 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_msort6(A,B):-msort(A,B).
my_odd7(A):-1 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_head11([H|_],H).
my_sumlist12(A,B):-sumlist(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_toupper14(A,B):-upcase_atom(A,B).
my_max_list15(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_even4/1).
prim(my_min_list5/2).
prim(my_msort6/2).
prim(my_odd7/1).
prim(my_lowercase8/1).
prim(my_tolower9/2).
prim(my_list_to_set10/2).
prim(my_head11/2).
prim(my_sumlist12/2).
prim(my_uppercase13/1).
prim(my_toupper14/2).
prim(my_max_list15/2).
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
p([['s','Y','B','u'],['M','x','i']],[['s','Y','B'],['M','x']]).
p([['O','j','v'],['p','z','w'],['d','j','D','r']],[['O','j'],['p','z'],['d','j','D']]).
p([['C','X','k','l'],['r','q','R']],[['C','X','k'],['r','q']]).
p([['o','r','w','f'],['X','s','r','Z'],['K','D','d'],['U','S','Y']],[['o','r','w'],['X','s','r'],['K','D'],['U','S']]).
p([['h','H','Z'],['U','Q','S'],['C','u','t']],[['h','H'],['U','Q'],['C','u']]).
q([['P','W','f'],['Y','b','w']],[['P','W'],['Y','b','w']]).
q([['U','V','b','Z'],['m','C','l','n'],['A','h','D']],[['U','V','b','Z'],['m','C','l'],['A','h','D']]).
q([['n','b','C'],['Q','K','O'],['k','D','u','c'],['I','Q','u','L']],[['n','b','C'],['Q','K'],['k','D','u','c'],['I','Q','u','L']]).
q([['y','T','D'],['d','g','m'],['w','O','r','K'],['F','A','Z']],[['y','T'],['d','g'],['w','O','r','K'],['F','A','Z']]).
q([['S','A','w','S'],['M','M','E','Y']],[['S','A','w','S'],['M','M','E']]).
