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

my_msort3(A,B):-msort(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_even5(A):-0 is A mod 2.
my_sumlist6(A,B):-sumlist(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_flatten8(A,B):-flatten(A,B).
my_max_list9(A,B):-max_list(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_min_list11(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_tolower4,[char,char]).
prim(my_even5,[int]).
prim(my_sumlist6,[list(int),int]).
prim(my_double7,[int,int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_max_list9,[list(int),int]).
prim(my_list_to_set10,[list(T),list(T)]).
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
p([['u','w','r'],['J','H','w'],['B','g','V','f'],['B','a','y','Y']],[['u','w'],['J','H'],['B','g','V'],['B','a','y']]).
p([['y','d','x','c'],['o','i','o','m'],['L','y','d'],['s','q','C']],[['y','d','x'],['o','i','o'],['L','y'],['s','q']]).
p([['X','x','N','z'],['A','m','B']],[['X','x','N'],['A','m']]).
p([['D','A','u','G'],['M','E','i'],['w','n','R','y'],['C','W','K']],[['D','A','u'],['M','E'],['w','n','R'],['C','W']]).
p([['t','q','V'],['E','P','R','I'],['i','Q','G']],[['t','q'],['E','P','R'],['i','Q']]).
q([['A','m','i'],['a','c','d']],[['A','m'],['a','c','d']]).
q([['Z','t','N','k'],['S','m','e'],['t','I','D']],[['Z','t','N','k'],['S','m','e'],['t','I']]).
q([['N','B','M','U'],['Y','H','P','p'],['v','C','f']],[['N','B','M'],['Y','H','P','p'],['v','C','f']]).
q([['Q','U','H'],['K','J','c','F']],[['Q','U','H'],['K','J','c']]).
q([['Y','H','d','R'],['k','I','a','d'],['Y','e','g'],['c','p','N','D']],[['Y','H','d'],['k','I','a'],['Y','e','g'],['c','p','N','D']]).
