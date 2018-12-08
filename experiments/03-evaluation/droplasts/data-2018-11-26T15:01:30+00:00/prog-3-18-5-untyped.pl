:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ3(A,B):-succ(A,B),B =< 10.
my_pred4(A,B):-succ(B,A),A > 0.
my_tolower5(A,B):-downcase_atom(A,B).
my_even6(A):-0 is A mod 2.
my_odd7(A):-1 is A mod 2.
my_uppercase8(A):-upcase_atom(A,A).
my_set9(A):-list_to_set(A,A).
my_msort10(A,B):-msort(A,B).
my_head11([H|_],H).
my_element12(A,B):-member(B,A).
my_len13(A,B):-length(A,B).
my_last14(A,B):-last(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_list_to_set17(A,B):-list_to_set(A,B).
my_double18(N,M):-M is 2*N,M =< 10.
my_min_list19(A,B):-min_list(A,B).
my_flatten20(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_pred4/2).
prim(my_tolower5/2).
prim(my_even6/1).
prim(my_odd7/1).
prim(my_uppercase8/1).
prim(my_set9/1).
prim(my_msort10/2).
prim(my_head11/2).
prim(my_element12/2).
prim(my_len13/2).
prim(my_last14/2).
prim(my_toupper15/2).
prim(my_sumlist16/2).
prim(my_list_to_set17/2).
prim(my_double18/2).
prim(my_min_list19/2).
prim(my_flatten20/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['w','Z','Z','V'],['d','m','x'],['x','V','b','O']],[['w','Z','Z'],['d','m'],['x','V','b']]).
p([['V','H','F','h'],['z','j','Y']],[['V','H','F'],['z','j']]).
p([['J','e','L','k'],['n','B','V','B'],['r','W','x','K'],['E','l','R','e']],[['J','e','L'],['n','B','V'],['r','W','x'],['E','l','R']]).
p([['V','e','C','m'],['M','P','P'],['k','x','R','Y'],['E','d','W']],[['V','e','C'],['M','P'],['k','x','R'],['E','d']]).
p([['O','h','b'],['L','h','Y']],[['O','h'],['L','h']]).
q([['Z','H','g'],['M','r','f','V'],['y','z','q'],['r','p','N']],[['Z','H'],['M','r','f'],['y','z','q'],['r','p','N']]).
q([['a','W','N'],['k','n','I'],['V','i','I'],['v','k','h']],[['a','W'],['k','n','I'],['V','i','I'],['v','k']]).
q([['J','F','R','U'],['e','u','Q','f']],[['J','F','R'],['e','u','Q','f']]).
q([['R','E','T','e'],['z','N','B','k'],['V','M','i','y'],['w','H','f','X']],[['R','E','T','e'],['z','N','B','k'],['V','M','i'],['w','H','f']]).
q([['u','K','V','x'],['w','J','s','R'],['n','M','V','q']],[['u','K','V'],['w','J','s','R'],['n','M','V','q']]).
