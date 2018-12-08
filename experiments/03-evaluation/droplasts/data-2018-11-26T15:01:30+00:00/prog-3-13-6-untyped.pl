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
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_last7(A,B):-last(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_max_list9(A,B):-max_list(A,B).
my_element10(A,B):-member(B,A).
my_double11(N,M):-M is 2*N,M =< 10.
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_flatten15(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_odd3/1).
prim(my_tolower4/2).
prim(my_lowercase5/1).
prim(my_uppercase6/1).
prim(my_last7/2).
prim(my_list_to_set8/2).
prim(my_max_list9/2).
prim(my_element10/2).
prim(my_double11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_toupper14/2).
prim(my_flatten15/2).
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
p([['p','h','y','B'],['C','X','I','r']],[['p','h','y'],['C','X','I']]).
p([['z','r','r','y'],['G','D','T'],['A','Z','u']],[['z','r','r'],['G','D'],['A','Z']]).
p([['r','J','C','n'],['i','y','t'],['G','g','L'],['E','o','D']],[['r','J','C'],['i','y'],['G','g'],['E','o']]).
p([['w','k','X'],['L','E','r','Q'],['p','O','E','z']],[['w','k'],['L','E','r'],['p','O','E']]).
p([['B','F','g','r'],['z','G','o','J'],['O','t','b','D']],[['B','F','g'],['z','G','o'],['O','t','b']]).
q([['L','v','c'],['q','w','C','D'],['r','A','O','A']],[['L','v','c'],['q','w','C'],['r','A','O','A']]).
q([['M','m','y','c'],['p','x','O','U']],[['M','m','y','c'],['p','x','O']]).
q([['u','D','i'],['m','h','y'],['C','q','L'],['m','U','D','s']],[['u','D','i'],['m','h','y'],['C','q','L'],['m','U','D']]).
q([['f','i','Y'],['z','N','Q'],['y','D','C']],[['f','i','Y'],['z','N','Q'],['y','D']]).
q([['U','S','j'],['m','Y','D','R']],[['U','S','j'],['m','Y','D']]).
