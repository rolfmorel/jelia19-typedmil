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

my_odd3(A):-1 is A mod 2.
my_max_list4(A,B):-max_list(A,B).
my_set5(A):-list_to_set(A,A).
my_len6(A,B):-length(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_pred10(A,B):-succ(B,A),A > 0.
my_min_list11(A,B):-min_list(A,B).
my_element12(A,B):-member(B,A).
my_double13(N,M):-M is 2*N,M =< 10.
my_list_to_set14(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
prim(my_max_list4,[list(int),int]).
prim(my_set5,[list(_)]).
prim(my_len6,[list(_),int]).
prim(my_lowercase7,[char]).
prim(my_last8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_min_list11,[list(int),int]).
prim(my_element12,[list(T),T]).
prim(my_double13,[int,int]).
prim(my_list_to_set14,[list(T),list(T)]).
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
p([['N','A','H','D'],['e','g','l'],['h','n','D']],[['N','A','H'],['e','g'],['h','n']]).
p([['N','N','B','A'],['N','p','q','C'],['W','t','O']],[['N','N','B'],['N','p','q'],['W','t']]).
p([['I','Y','c'],['W','X','s']],[['I','Y'],['W','X']]).
p([['o','X','K'],['q','Z','j','W'],['U','q','H'],['z','D','G','l']],[['o','X'],['q','Z','j'],['U','q'],['z','D','G']]).
p([['c','K','j','H'],['v','u','C','n'],['B','Q','m','U'],['F','P','X']],[['c','K','j'],['v','u','C'],['B','Q','m'],['F','P']]).
q([['s','C','l','D'],['e','T','j'],['t','Y','F'],['V','L','p','n']],[['s','C','l'],['e','T'],['t','Y','F'],['V','L','p','n']]).
q([['r','n','M','f'],['v','M','o'],['A','X','t','M'],['L','v','O']],[['r','n','M'],['v','M'],['A','X','t','M'],['L','v','O']]).
q([['N','Y','w'],['o','P','l'],['b','L','i']],[['N','Y','w'],['o','P'],['b','L','i']]).
q([['y','Q','b','o'],['Y','H','z','O']],[['y','Q','b'],['Y','H','z','O']]).
q([['F','t','W','U'],['r','H','d']],[['F','t','W'],['r','H','d']]).
