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

my_min_list3(A,B):-min_list(A,B).
my_flatten4(A,B):-flatten(A,B).
my_element5(A,B):-member(B,A).
my_uppercase6(A):-upcase_atom(A,A).
my_head7([H|_],H).
my_lowercase8(A):-downcase_atom(A,A).
my_odd9(A):-1 is A mod 2.
my_list_to_set10(A,B):-list_to_set(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_len12(A,B):-length(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_msort14(A,B):-msort(A,B).
my_set15(A):-list_to_set(A,A).
my_sumlist16(A,B):-sumlist(A,B).
my_tolower17(A,B):-downcase_atom(A,B).
my_max_list18(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_min_list3/2).
prim(my_flatten4/2).
prim(my_element5/2).
prim(my_uppercase6/1).
prim(my_head7/2).
prim(my_lowercase8/1).
prim(my_odd9/1).
prim(my_list_to_set10/2).
prim(my_pred11/2).
prim(my_len12/2).
prim(my_msort14/2).
prim(my_set15/1).
prim(my_sumlist16/2).
prim(my_tolower17/2).
prim(my_max_list18/2).
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
p([['i','k','D','l'],['D','K','k','J'],['E','V','J','C'],['b','J','y']],[['i','k','D'],['D','K','k'],['E','V','J'],['b','J']]).
p([['F','r','P','C'],['F','F','F','I'],['k','N','N'],['u','F','j']],[['F','r','P'],['F','F','F'],['k','N'],['u','F']]).
p([['y','E','c','L'],['z','J','u'],['v','K','K'],['S','u','U']],[['y','E','c'],['z','J'],['v','K'],['S','u']]).
p([['G','j','K','c'],['J','e','I'],['C','d','y','s']],[['G','j','K'],['J','e'],['C','d','y']]).
p([['y','I','N','n'],['Q','j','A','c'],['H','a','d','J']],[['y','I','N'],['Q','j','A'],['H','a','d']]).
q([['T','C','X'],['g','j','j'],['M','I','Y'],['x','r','u']],[['T','C'],['g','j','j'],['M','I','Y'],['x','r']]).
q([['x','K','P'],['k','N','L'],['J','T','V']],[['x','K'],['k','N','L'],['J','T','V']]).
q([['o','R','c'],['y','b','i','E'],['w','v','p']],[['o','R'],['y','b','i','E'],['w','v','p']]).
q([['o','R','X','U'],['e','T','y']],[['o','R','X'],['e','T','y']]).
q([['s','X','e'],['w','T','X']],[['s','X'],['w','T','X']]).
