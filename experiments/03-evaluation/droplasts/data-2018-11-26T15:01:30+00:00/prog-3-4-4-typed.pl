:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_list_to_set3(A,B):-list_to_set(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_element5(A,B):-member(B,A).
my_lowercase6(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_uppercase4,[char]).
prim(my_element5,[list(T),T]).
prim(my_lowercase6,[char]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['l','d','M'],['X','F','O']],[['l','d'],['X','F']]).
p([['F','w','F'],['z','P','r','w']],[['F','w'],['z','P','r']]).
p([['t','P','a','w'],['J','u','A','j'],['B','M','g']],[['t','P','a'],['J','u','A'],['B','M']]).
p([['g','w','r'],['Q','u','L'],['i','u','x','d']],[['g','w'],['Q','u'],['i','u','x']]).
p([['B','f','Y','g'],['k','K','L','V'],['i','I','b','Y'],['D','N','x']],[['B','f','Y'],['k','K','L'],['i','I','b'],['D','N']]).
q([['X','W','o'],['t','I','f','O']],[['X','W','o'],['t','I','f']]).
q([['K','Y','h'],['y','f','f'],['t','r','J','m'],['e','O','X']],[['K','Y','h'],['y','f'],['t','r','J'],['e','O','X']]).
q([['V','D','A'],['f','m','M','H'],['B','m','x','B']],[['V','D'],['f','m','M','H'],['B','m','x','B']]).
q([['r','E','Z','m'],['K','P','r','G'],['w','j','M','e']],[['r','E','Z','m'],['K','P','r'],['w','j','M','e']]).
q([['F','w','E'],['f','e','a','o'],['U','D','Q','T']],[['F','w','E'],['f','e','a'],['U','D','Q','T']]).
