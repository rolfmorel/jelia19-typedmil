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

my_succ3(A,B):-succ(A,B),B =< 10.
my_len4(A,B):-length(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_pred6(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_succ3,[int,int]).
prim(my_len4,[list(_),int]).
prim(my_double5,[int,int]).
prim(my_pred6,[int,int]).
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
p([['L','X','F'],['s','B','o']],[['L','X'],['s','B']]).
p([['A','N','t'],['D','b','C','K'],['I','q','e','b'],['p','r','I']],[['A','N'],['D','b','C'],['I','q','e'],['p','r']]).
p([['F','P','d','X'],['p','g','T'],['k','p','L']],[['F','P','d'],['p','g'],['k','p']]).
p([['r','S','M'],['X','a','w']],[['r','S'],['X','a']]).
p([['z','v','a','D'],['Q','n','E','O'],['d','c','s']],[['z','v','a'],['Q','n','E'],['d','c']]).
q([['T','h','Q','E'],['Q','h','j'],['p','M','z','j']],[['T','h','Q','E'],['Q','h','j'],['p','M','z']]).
q([['G','L','O'],['b','M','P'],['r','d','X','d'],['A','j','b','b']],[['G','L','O'],['b','M','P'],['r','d','X'],['A','j','b','b']]).
q([['F','y','r','d'],['p','L','f']],[['F','y','r','d'],['p','L']]).
q([['a','t','u'],['K','w','x'],['Q','x','V','Q'],['F','q','x']],[['a','t','u'],['K','w','x'],['Q','x','V','Q'],['F','q']]).
q([['V','D','z','I'],['O','H','R'],['r','e','N']],[['V','D','z'],['O','H','R'],['r','e','N']]).
