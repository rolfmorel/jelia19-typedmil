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
my_last4(A,B):-last(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_msort6(A,B):-msort(A,B).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_uppercase9(A):-upcase_atom(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_pred12(A,B):-succ(B,A),A > 0.
my_succ13(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_last4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_msort6,[list(int),list(int)]).
prim(my_set7,[list(_)]).
prim(my_element8,[list(T),T]).
prim(my_uppercase9,[char]).
prim(my_toupper10,[char,char]).
prim(my_double11,[int,int]).
prim(my_pred12,[int,int]).
prim(my_succ13,[int,int]).
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
p([['H','r','B'],['h','h','u','E'],['a','p','G','K'],['I','p','d']],[['H','r'],['h','h','u'],['a','p','G'],['I','p']]).
p([['b','B','A','V'],['o','j','o'],['o','M','v'],['M','n','A']],[['b','B','A'],['o','j'],['o','M'],['M','n']]).
p([['G','n','H'],['N','r','n'],['h','Z','P']],[['G','n'],['N','r'],['h','Z']]).
p([['R','U','V'],['d','X','m','m'],['Q','h','W'],['S','K','I','v']],[['R','U'],['d','X','m'],['Q','h'],['S','K','I']]).
p([['c','k','j','X'],['u','R','C']],[['c','k','j'],['u','R']]).
q([['H','Z','I','Z'],['r','e','L','T'],['T','X','V']],[['H','Z','I'],['r','e','L','T'],['T','X','V']]).
q([['h','p','S'],['i','v','m']],[['h','p'],['i','v','m']]).
q([['u','U','s'],['O','h','c','s'],['P','F','G'],['M','m','m','z']],[['u','U'],['O','h','c'],['P','F','G'],['M','m','m','z']]).
q([['q','z','E','V'],['x','q','H'],['I','v','q','J'],['q','e','Y','r']],[['q','z','E','V'],['x','q','H'],['I','v','q'],['q','e','Y','r']]).
q([['W','C','g'],['r','Q','r','O']],[['W','C'],['r','Q','r','O']]).
