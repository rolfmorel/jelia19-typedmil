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

my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_last3,[list(T),T]).
prim(my_succ4,[int,int]).
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
p([['N','E','a'],['i','z','p','h']],[['N','E'],['i','z','p']]).
p([['r','m','n'],['N','J','F']],[['r','m'],['N','J']]).
p([['Y','U','J'],['Z','e','i'],['S','T','R','r'],['Z','d','n','O']],[['Y','U'],['Z','e'],['S','T','R'],['Z','d','n']]).
p([['H','Q','N','k'],['K','e','G']],[['H','Q','N'],['K','e']]).
p([['V','Z','Y','f'],['y','O','u','Y'],['Y','s','m','P'],['G','r','U','T']],[['V','Z','Y'],['y','O','u'],['Y','s','m'],['G','r','U']]).
q([['S','u','E'],['T','s','F','U']],[['S','u'],['T','s','F','U']]).
q([['n','T','o'],['f','r','d']],[['n','T','o'],['f','r']]).
q([['b','z','Q'],['j','h','p'],['u','T','F','L']],[['b','z','Q'],['j','h'],['u','T','F','L']]).
q([['S','c','b','V'],['X','a','F','J'],['X','l','x']],[['S','c','b','V'],['X','a','F'],['X','l','x']]).
q([['s','T','j'],['r','o','A'],['H','a','W','N'],['B','v','B','G']],[['s','T'],['r','o'],['H','a','W','N'],['B','v','B','G']]).
