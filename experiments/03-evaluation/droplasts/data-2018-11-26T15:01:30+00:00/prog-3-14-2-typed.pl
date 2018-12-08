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

my_max_list3(A,B):-max_list(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_toupper5(A,B):-upcase_atom(A,B).
my_flatten6(A,B):-flatten(A,B).
my_last7(A,B):-last(A,B).
my_len8(A,B):-length(A,B).
my_odd9(A):-1 is A mod 2.
my_succ10(A,B):-succ(A,B),B =< 10.
my_pred11(A,B):-succ(B,A),A > 0.
my_even12(A):-0 is A mod 2.
my_list_to_set13(A,B):-list_to_set(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_lowercase15(A):-downcase_atom(A,A).
my_element16(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_uppercase4,[char]).
prim(my_toupper5,[char,char]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_last7,[list(T),T]).
prim(my_len8,[list(_),int]).
prim(my_odd9,[int]).
prim(my_succ10,[int,int]).
prim(my_pred11,[int,int]).
prim(my_even12,[int]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_tolower14,[char,char]).
prim(my_lowercase15,[char]).
prim(my_element16,[list(T),T]).
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
p([['i','D','U'],['d','U','D','V'],['j','b','b'],['G','k','M','S']],[['i','D'],['d','U','D'],['j','b'],['G','k','M']]).
p([['z','v','F'],['I','a','E','U'],['g','p','W']],[['z','v'],['I','a','E'],['g','p']]).
p([['D','j','T'],['j','i','Y','n']],[['D','j'],['j','i','Y']]).
p([['N','o','v'],['w','z','Q']],[['N','o'],['w','z']]).
p([['H','D','K'],['W','A','T','M'],['c','u','D'],['z','V','S','y']],[['H','D'],['W','A','T'],['c','u'],['z','V','S']]).
q([['M','Q','B','Y'],['j','k','Q','z'],['C','h','v','E'],['q','l','L','O']],[['M','Q','B','Y'],['j','k','Q'],['C','h','v','E'],['q','l','L','O']]).
q([['A','F','b'],['N','X','U','c'],['L','B','e'],['t','P','X']],[['A','F'],['N','X','U','c'],['L','B','e'],['t','P']]).
q([['M','o','Z'],['n','o','A'],['j','y','O']],[['M','o','Z'],['n','o','A'],['j','y']]).
q([['J','G','u'],['r','L','P','T'],['z','M','W']],[['J','G','u'],['r','L','P','T'],['z','M']]).
q([['q','w','o'],['I','B','H']],[['q','w','o'],['I','B']]).
