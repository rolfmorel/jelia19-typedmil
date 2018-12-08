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

my_even3(A):-0 is A mod 2.
my_uppercase4(A):-upcase_atom(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B),B =< 10.
my_lowercase9(A):-downcase_atom(A,A).
my_odd10(A):-1 is A mod 2.
my_min_list11(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_uppercase4,[char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_pred6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_lowercase9,[char]).
prim(my_odd10,[int]).
prim(my_min_list11,[list(int),int]).
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
p([['H','a','J','x'],['D','a','d'],['N','I','D'],['o','J','M','z']],[['H','a','J'],['D','a'],['N','I'],['o','J','M']]).
p([['J','x','y','N'],['X','J','H']],[['J','x','y'],['X','J']]).
p([['Q','i','J','N'],['z','n','L'],['P','X','F','o'],['X','a','q']],[['Q','i','J'],['z','n'],['P','X','F'],['X','a']]).
p([['u','s','i'],['r','c','K'],['A','M','N','j']],[['u','s'],['r','c'],['A','M','N']]).
p([['g','r','s','Z'],['R','D','U','q'],['B','n','B','O'],['j','y','y','Z']],[['g','r','s'],['R','D','U'],['B','n','B'],['j','y','y']]).
q([['i','q','e','y'],['w','e','F']],[['i','q','e','y'],['w','e']]).
q([['k','M','P'],['i','J','E','a'],['e','t','y'],['M','E','v','R']],[['k','M','P'],['i','J','E','a'],['e','t'],['M','E','v']]).
q([['i','o','b','U'],['O','a','n','H'],['k','f','U'],['E','Q','Y']],[['i','o','b'],['O','a','n','H'],['k','f','U'],['E','Q']]).
q([['h','l','V','P'],['t','d','K','s'],['P','X','a','M'],['o','p','E']],[['h','l','V'],['t','d','K','s'],['P','X','a'],['o','p','E']]).
q([['g','R','E','U'],['n','S','l','R']],[['g','R','E','U'],['n','S','l']]).
