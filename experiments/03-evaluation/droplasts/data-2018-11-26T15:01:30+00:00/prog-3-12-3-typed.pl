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
my_double4(N,M):-M is 2*N,M =< 10.
my_even5(A):-0 is A mod 2.
my_toupper6(A,B):-upcase_atom(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_odd10(A):-1 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).
my_list_to_set13(A,B):-list_to_set(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_double4,[int,int]).
prim(my_even5,[int]).
prim(my_toupper6,[char,char]).
prim(my_tolower7,[char,char]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_odd10,[int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_set12,[list(_)]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_pred14,[int,int]).
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
p([['K','I','V'],['U','Q','c'],['d','O','q','v'],['W','Q','N']],[['K','I'],['U','Q'],['d','O','q'],['W','Q']]).
p([['M','r','z'],['B','c','q'],['N','J','L','F']],[['M','r'],['B','c'],['N','J','L']]).
p([['U','C','x','L'],['z','P','C'],['N','l','V','o'],['p','T','O']],[['U','C','x'],['z','P'],['N','l','V'],['p','T']]).
p([['x','D','S'],['h','B','D'],['A','r','N','N']],[['x','D'],['h','B'],['A','r','N']]).
p([['L','d','g','P'],['C','d','b'],['s','t','y']],[['L','d','g'],['C','d'],['s','t']]).
q([['X','L','N','X'],['X','B','E','z'],['k','o','u','l'],['I','d','S','H']],[['X','L','N','X'],['X','B','E','z'],['k','o','u','l'],['I','d','S']]).
q([['g','I','F'],['C','f','A']],[['g','I','F'],['C','f']]).
q([['A','E','k','t'],['o','j','S'],['z','M','U'],['S','p','K','S']],[['A','E','k'],['o','j','S'],['z','M','U'],['S','p','K','S']]).
q([['N','w','r','B'],['L','u','n']],[['N','w','r'],['L','u','n']]).
q([['F','s','P'],['X','s','o']],[['F','s'],['X','s','o']]).
