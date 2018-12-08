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
my_odd4(A):-1 is A mod 2.
my_succ5(A,B):-succ(A,B),B =< 10.
my_max_list6(A,B):-max_list(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_element8(A,B):-member(B,A).
my_head9([H|_],H).
my_msort10(A,B):-msort(A,B).
my_flatten11(A,B):-flatten(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_set13(A):-list_to_set(A,A).
my_min_list14(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_odd4,[int]).
prim(my_succ5,[int,int]).
prim(my_max_list6,[list(int),int]).
prim(my_toupper7,[char,char]).
prim(my_element8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_msort10,[list(int),list(int)]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_sumlist12,[list(int),int]).
prim(my_set13,[list(_)]).
prim(my_min_list14,[list(int),int]).
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
p([['J','T','W','V'],['k','Y','U','z'],['J','O','O','y']],[['J','T','W'],['k','Y','U'],['J','O','O']]).
p([['L','o','F'],['h','A','a','b'],['b','i','H','r']],[['L','o'],['h','A','a'],['b','i','H']]).
p([['q','E','U','m'],['B','O','j'],['X','Y','A']],[['q','E','U'],['B','O'],['X','Y']]).
p([['a','A','X','G'],['h','F','j','O']],[['a','A','X'],['h','F','j']]).
p([['W','E','H'],['s','j','o','h'],['P','u','K'],['V','r','B','O']],[['W','E'],['s','j','o'],['P','u'],['V','r','B']]).
q([['N','U','W'],['d','F','W','w']],[['N','U'],['d','F','W','w']]).
q([['a','L','o','k'],['z','e','u'],['K','Q','u','n'],['S','M','K','l']],[['a','L','o','k'],['z','e'],['K','Q','u','n'],['S','M','K','l']]).
q([['y','q','L'],['G','P','p']],[['y','q','L'],['G','P']]).
q([['S','h','s'],['f','d','l','I']],[['S','h','s'],['f','d','l']]).
q([['z','Y','W'],['M','V','i'],['u','a','P'],['b','O','C']],[['z','Y','W'],['M','V'],['u','a','P'],['b','O']]).
