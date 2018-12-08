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

my_sumlist3(A,B):-sumlist(A,B).
my_msort4(A,B):-msort(A,B).
my_set5(A):-list_to_set(A,A).
my_pred6(A,B):-succ(B,A),A > 0.
my_flatten7(A,B):-flatten(A,B).
my_even8(A):-0 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A).
my_head10([H|_],H).
my_uppercase11(A):-upcase_atom(A,A).
my_double12(N,M):-M is 2*N,M =< 10.
my_max_list13(A,B):-max_list(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B).
my_min_list16(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_set5,[list(_)]).
prim(my_pred6,[int,int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_even8,[int]).
prim(my_lowercase9,[char]).
prim(my_head10,[list(T),T]).
prim(my_uppercase11,[char]).
prim(my_double12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_tolower15,[char,char]).
prim(my_min_list16,[list(int),int]).
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
p([['p','X','U','f'],['X','B','s','J']],[['p','X','U'],['X','B','s']]).
p([['t','k','N','d'],['L','E','u','H'],['B','E','K','X']],[['t','k','N'],['L','E','u'],['B','E','K']]).
p([['M','m','f'],['k','x','X','l'],['J','l','C','X']],[['M','m'],['k','x','X'],['J','l','C']]).
p([['c','k','M'],['W','R','s','H'],['l','T','M','L']],[['c','k'],['W','R','s'],['l','T','M']]).
p([['B','g','o'],['G','Q','Y','i'],['f','A','s','n'],['q','Q','W','n']],[['B','g'],['G','Q','Y'],['f','A','s'],['q','Q','W']]).
q([['p','Y','z','Q'],['S','c','A'],['K','M','Z','x'],['N','r','r']],[['p','Y','z','Q'],['S','c'],['K','M','Z','x'],['N','r','r']]).
q([['K','s','A','S'],['m','Z','w','K'],['R','u','d','V']],[['K','s','A'],['m','Z','w','K'],['R','u','d','V']]).
q([['E','Q','d','n'],['F','S','Y']],[['E','Q','d'],['F','S','Y']]).
q([['v','e','N','S'],['H','x','X']],[['v','e','N'],['H','x','X']]).
q([['X','I','O','X'],['s','G','F','n'],['e','o','G'],['O','s','j','o']],[['X','I','O'],['s','G','F','n'],['e','o','G'],['O','s','j']]).
