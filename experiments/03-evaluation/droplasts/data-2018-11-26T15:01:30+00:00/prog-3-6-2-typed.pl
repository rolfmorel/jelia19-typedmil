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

my_uppercase3(A):-upcase_atom(A,A).
my_odd4(A):-1 is A mod 2.
my_flatten5(A,B):-flatten(A,B).
my_msort6(A,B):-msort(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_even8(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_odd4,[int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_msort6,[list(int),list(int)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_even8,[int]).
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
p([['x','G','b'],['i','w','P','Y'],['z','E','n','b']],[['x','G'],['i','w','P'],['z','E','n']]).
p([['i','I','a'],['z','a','k','w']],[['i','I'],['z','a','k']]).
p([['g','N','R'],['d','A','q','P'],['c','h','t','a']],[['g','N'],['d','A','q'],['c','h','t']]).
p([['K','H','o'],['l','m','a','K'],['I','A','e','G']],[['K','H'],['l','m','a'],['I','A','e']]).
p([['u','e','n'],['s','B','n'],['y','L','C','T']],[['u','e'],['s','B'],['y','L','C']]).
q([['J','X','F'],['X','Q','I'],['d','x','t'],['v','t','V','z']],[['J','X','F'],['X','Q','I'],['d','x'],['v','t','V','z']]).
q([['m','i','d','x'],['b','B','k','b'],['I','k','z']],[['m','i','d'],['b','B','k','b'],['I','k','z']]).
q([['p','l','u','n'],['n','Z','Y','Q'],['z','C','J','t']],[['p','l','u'],['n','Z','Y','Q'],['z','C','J','t']]).
q([['U','w','n','x'],['D','k','R','c']],[['U','w','n','x'],['D','k','R']]).
q([['K','T','B','b'],['B','c','R','G'],['H','T','D','v'],['O','a','X','z']],[['K','T','B','b'],['B','c','R'],['H','T','D'],['O','a','X','z']]).
