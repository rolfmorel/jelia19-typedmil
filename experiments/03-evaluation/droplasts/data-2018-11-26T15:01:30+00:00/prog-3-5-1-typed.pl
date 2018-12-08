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

my_head3([H|_],H).
my_even4(A):-0 is A mod 2.
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_uppercase7(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_head3,[list(T),T]).
prim(my_even4,[int]).
prim(my_len5,[list(_),int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_uppercase7,[char]).
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
p([['u','x','N'],['z','E','p']],[['u','x'],['z','E']]).
p([['d','x','i'],['C','B','o','R'],['u','r','m','C']],[['d','x'],['C','B','o'],['u','r','m']]).
p([['O','s','X','v'],['W','W','c'],['g','x','j'],['q','m','j','x']],[['O','s','X'],['W','W'],['g','x'],['q','m','j']]).
p([['p','C','V'],['F','U','x'],['z','p','O','T']],[['p','C'],['F','U'],['z','p','O']]).
p([['S','h','y'],['N','G','M','n'],['g','p','N','z']],[['S','h'],['N','G','M'],['g','p','N']]).
q([['b','y','a'],['S','z','M']],[['b','y'],['S','z','M']]).
q([['e','C','q'],['I','N','O','Y'],['Q','h','N']],[['e','C','q'],['I','N','O'],['Q','h','N']]).
q([['a','N','M'],['E','B','q'],['r','X','t','C']],[['a','N'],['E','B','q'],['r','X','t','C']]).
q([['f','C','c','k'],['G','y','Q','u']],[['f','C','c'],['G','y','Q','u']]).
q([['d','y','H'],['i','i','B'],['q','x','r','c'],['j','m','a']],[['d','y','H'],['i','i'],['q','x','r','c'],['j','m','a']]).
