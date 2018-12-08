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

my_len3(A,B):-length(A,B).
my_element4(A,B):-member(B,A).
my_tolower5(A,B):-downcase_atom(A,B).
my_head6([H|_],H).
my_msort7(A,B):-msort(A,B).
my_odd8(A):-1 is A mod 2.
my_even9(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_element4,[list(T),T]).
prim(my_tolower5,[char,char]).
prim(my_head6,[list(T),T]).
prim(my_msort7,[list(int),list(int)]).
prim(my_odd8,[int]).
prim(my_even9,[int]).
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
p([['v','Z','d'],['V','Q','L'],['x','i','O','b']],[['v','Z'],['V','Q'],['x','i','O']]).
p([['i','j','E','z'],['Z','H','X'],['E','w','U','U'],['H','x','q','W']],[['i','j','E'],['Z','H'],['E','w','U'],['H','x','q']]).
p([['A','I','x','E'],['O','q','C'],['X','i','e','j']],[['A','I','x'],['O','q'],['X','i','e']]).
p([['H','t','z'],['f','h','o'],['x','u','X'],['j','b','L','B']],[['H','t'],['f','h'],['x','u'],['j','b','L']]).
p([['m','U','L','u'],['a','S','L','c']],[['m','U','L'],['a','S','L']]).
q([['F','N','G','N'],['f','W','R','c'],['k','w','W','b'],['F','i','u','Z']],[['F','N','G'],['f','W','R','c'],['k','w','W','b'],['F','i','u','Z']]).
q([['V','L','X'],['W','a','w','D']],[['V','L'],['W','a','w','D']]).
q([['P','C','j'],['w','b','F','b'],['c','s','u','F']],[['P','C'],['w','b','F','b'],['c','s','u','F']]).
q([['V','b','Y','U'],['h','x','S'],['C','C','V','Z'],['G','V','N','Q']],[['V','b','Y'],['h','x','S'],['C','C','V','Z'],['G','V','N','Q']]).
q([['D','o','J','n'],['W','Z','L'],['G','q','c'],['Z','c','F']],[['D','o','J','n'],['W','Z'],['G','q','c'],['Z','c','F']]).
