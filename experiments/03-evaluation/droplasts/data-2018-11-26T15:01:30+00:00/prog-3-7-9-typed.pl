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
my_double4(N,M):-M is 2*N,M =< 10.
my_max_list5(A,B):-max_list(A,B).
my_even6(A):-0 is A mod 2.
my_odd7(A):-1 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_last9(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_double4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_even6,[int]).
prim(my_odd7,[int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_last9,[list(T),T]).
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
p([['d','G','b'],['y','w','E','o'],['X','Z','H','j']],[['d','G'],['y','w','E'],['X','Z','H']]).
p([['m','h','F'],['D','F','T'],['w','G','G']],[['m','h'],['D','F'],['w','G']]).
p([['M','e','R','g'],['v','y','M','N'],['W','g','e']],[['M','e','R'],['v','y','M'],['W','g']]).
p([['t','i','P','P'],['M','G','C','k'],['A','B','z']],[['t','i','P'],['M','G','C'],['A','B']]).
p([['u','W','r'],['u','f','R'],['i','W','p'],['W','N','Z','n']],[['u','W'],['u','f'],['i','W'],['W','N','Z']]).
q([['k','L','e'],['i','m','M','x'],['O','p','k','Q'],['c','j','m']],[['k','L','e'],['i','m','M','x'],['O','p','k'],['c','j','m']]).
q([['a','K','E'],['k','R','F']],[['a','K'],['k','R','F']]).
q([['M','B','z','B'],['v','R','n','X'],['c','c','x'],['h','a','V','L']],[['M','B','z','B'],['v','R','n','X'],['c','c'],['h','a','V']]).
q([['u','V','e'],['I','F','C']],[['u','V'],['I','F','C']]).
q([['n','X','B','d'],['k','v','e'],['A','i','y','L']],[['n','X','B','d'],['k','v','e'],['A','i','y']]).
