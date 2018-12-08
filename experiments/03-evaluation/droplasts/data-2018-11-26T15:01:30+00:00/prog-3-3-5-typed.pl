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

my_odd3(A):-1 is A mod 2.
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
prim(my_head4,[list(T),T]).
prim(my_len5,[list(_),int]).
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
p([['n','M','Q','T'],['w','J','B','E'],['c','n','J']],[['n','M','Q'],['w','J','B'],['c','n']]).
p([['c','W','M','k'],['y','n','k'],['M','P','B']],[['c','W','M'],['y','n'],['M','P']]).
p([['V','r','i'],['c','k','K','u']],[['V','r'],['c','k','K']]).
p([['Y','e','J'],['m','X','z','J'],['G','V','S']],[['Y','e'],['m','X','z'],['G','V']]).
p([['h','L','i'],['w','t','U','G']],[['h','L'],['w','t','U']]).
q([['k','l','u','N'],['E','G','N'],['w','X','O'],['U','N','K']],[['k','l','u'],['E','G','N'],['w','X','O'],['U','N','K']]).
q([['K','N','W'],['s','K','D'],['P','S','M'],['X','s','g']],[['K','N'],['s','K'],['P','S','M'],['X','s','g']]).
q([['L','M','m'],['E','j','a','K'],['V','U','l']],[['L','M','m'],['E','j','a','K'],['V','U']]).
q([['S','f','O'],['r','r','h'],['a','q','C','l'],['V','t','b']],[['S','f','O'],['r','r'],['a','q','C','l'],['V','t','b']]).
q([['g','z','V','N'],['B','F','h']],[['g','z','V'],['B','F','h']]).
