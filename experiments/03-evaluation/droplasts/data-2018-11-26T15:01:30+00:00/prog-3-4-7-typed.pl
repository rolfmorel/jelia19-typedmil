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
my_toupper4(A,B):-upcase_atom(A,B).
my_odd5(A):-1 is A mod 2.
my_uppercase6(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_toupper4,[char,char]).
prim(my_odd5,[int]).
prim(my_uppercase6,[char]).
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
p([['k','y','F'],['O','c','L','R']],[['k','y'],['O','c','L']]).
p([['y','p','R','F'],['s','Y','g','I']],[['y','p','R'],['s','Y','g']]).
p([['V','u','r','a'],['m','Z','E','G']],[['V','u','r'],['m','Z','E']]).
p([['Y','z','x','g'],['h','k','G','R'],['N','q','O']],[['Y','z','x'],['h','k','G'],['N','q']]).
p([['M','W','y','F'],['A','r','t','k'],['D','i','F']],[['M','W','y'],['A','r','t'],['D','i']]).
q([['V','J','V','a'],['H','R','d'],['p','c','k'],['d','o','H','N']],[['V','J','V','a'],['H','R'],['p','c','k'],['d','o','H','N']]).
q([['G','O','d'],['x','a','R','E']],[['G','O','d'],['x','a','R']]).
q([['i','T','g','D'],['J','d','s','J'],['t','r','t','i']],[['i','T','g','D'],['J','d','s'],['t','r','t','i']]).
q([['G','p','D'],['x','I','i'],['g','i','J','W']],[['G','p','D'],['x','I'],['g','i','J','W']]).
q([['h','R','l'],['Q','h','q','s'],['U','E','M','v']],[['h','R'],['Q','h','q','s'],['U','E','M','v']]).
