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
my_tolower4(A,B):-downcase_atom(A,B).
my_len5(A,B):-length(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_even7(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_head3,[list(T),T]).
prim(my_tolower4,[char,char]).
prim(my_len5,[list(_),int]).
prim(my_toupper6,[char,char]).
prim(my_even7,[int]).
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
p([['h','i','N','V'],['y','W','D','t'],['S','f','C','O']],[['h','i','N'],['y','W','D'],['S','f','C']]).
p([['e','E','A','X'],['B','T','x']],[['e','E','A'],['B','T']]).
p([['A','a','Y'],['B','M','q','k'],['o','G','k','p'],['q','e','o']],[['A','a'],['B','M','q'],['o','G','k'],['q','e']]).
p([['r','M','R'],['W','w','Q'],['X','W','t','o'],['k','e','M']],[['r','M'],['W','w'],['X','W','t'],['k','e']]).
p([['g','w','H','f'],['A','r','Q','r'],['G','x','d'],['R','d','Y']],[['g','w','H'],['A','r','Q'],['G','x'],['R','d']]).
q([['R','O','o'],['G','Z','n'],['F','n','u']],[['R','O','o'],['G','Z','n'],['F','n']]).
q([['a','c','z'],['U','O','x'],['W','b','G','j'],['Q','a','t','E']],[['a','c','z'],['U','O','x'],['W','b','G'],['Q','a','t','E']]).
q([['u','a','g','w'],['k','M','P']],[['u','a','g'],['k','M','P']]).
q([['U','N','G'],['U','O','s']],[['U','N','G'],['U','O']]).
q([['K','I','Y'],['o','Q','j'],['L','q','R','n']],[['K','I','Y'],['o','Q'],['L','q','R','n']]).
