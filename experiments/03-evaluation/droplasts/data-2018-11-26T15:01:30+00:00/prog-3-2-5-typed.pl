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

my_pred3(A,B):-succ(B,A),A > 0.
my_last4(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_last4,[list(T),T]).
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
p([['u','m','v','A'],['B','l','n','b'],['l','f','m']],[['u','m','v'],['B','l','n'],['l','f']]).
p([['N','R','c','L'],['l','V','O'],['x','i','R'],['t','f','B']],[['N','R','c'],['l','V'],['x','i'],['t','f']]).
p([['O','K','m'],['j','t','B']],[['O','K'],['j','t']]).
p([['E','x','j'],['P','V','o']],[['E','x'],['P','V']]).
p([['m','L','t','d'],['L','J','o'],['A','T','Z'],['c','G','k','d']],[['m','L','t'],['L','J'],['A','T'],['c','G','k']]).
q([['Y','h','b'],['O','B','T'],['P','b','H','j'],['H','v','z']],[['Y','h','b'],['O','B'],['P','b','H','j'],['H','v','z']]).
q([['C','d','O','T'],['J','O','R']],[['C','d','O'],['J','O','R']]).
q([['v','D','s'],['m','b','h'],['V','R','B'],['u','a','i','S']],[['v','D','s'],['m','b','h'],['V','R'],['u','a','i']]).
q([['e','d','S'],['n','s','d'],['J','H','P','O'],['s','u','n','G']],[['e','d','S'],['n','s','d'],['J','H','P'],['s','u','n','G']]).
q([['g','C','e','M'],['W','y','f','R'],['W','M','H','k']],[['g','C','e','M'],['W','y','f'],['W','M','H','k']]).
