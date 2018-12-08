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
my_msort4(A,B):-msort(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).
my_flatten9(A,B):-flatten(A,B).
my_max_list10(A,B):-max_list(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_set12(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_max_list10,[list(int),int]).
prim(my_uppercase11,[char]).
prim(my_set12,[list(_)]).
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
p([['h','y','R','w'],['T','X','T']],[['h','y','R'],['T','X']]).
p([['z','M','V'],['H','c','K','i']],[['z','M'],['H','c','K']]).
p([['x','M','N'],['q','p','L','e'],['O','U','G']],[['x','M'],['q','p','L'],['O','U']]).
p([['v','t','l'],['n','J','M','g']],[['v','t'],['n','J','M']]).
p([['s','D','S','X'],['r','S','J','z'],['G','n','F','C']],[['s','D','S'],['r','S','J'],['G','n','F']]).
q([['Q','k','b'],['s','d','c']],[['Q','k'],['s','d','c']]).
q([['W','r','H','F'],['I','Q','u'],['H','v','I','e'],['g','r','Q','U']],[['W','r','H','F'],['I','Q','u'],['H','v','I'],['g','r','Q','U']]).
q([['m','G','D'],['v','L','x']],[['m','G','D'],['v','L']]).
q([['c','c','i'],['Z','y','L','C'],['q','z','R','z'],['S','N','O','c']],[['c','c','i'],['Z','y','L','C'],['q','z','R'],['S','N','O','c']]).
q([['E','o','M','Y'],['f','y','J','a'],['S','s','G']],[['E','o','M'],['f','y','J','a'],['S','s','G']]).
