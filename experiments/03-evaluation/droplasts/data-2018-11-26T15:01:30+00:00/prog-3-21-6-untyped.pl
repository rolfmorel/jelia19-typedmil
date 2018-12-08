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

my_head3([H|_],H).
my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_flatten6(A,B):-flatten(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_even8(A):-0 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_toupper11(A,B):-upcase_atom(A,B).

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

my_tolower13(A,B):-downcase_atom(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_set15(A):-list_to_set(A,A).
my_msort16(A,B):-msort(A,B).
my_element17(A,B):-member(B,A).
my_double18(N,M):-M is 2*N,M =< 10.
my_max_list19(A,B):-max_list(A,B).
my_odd20(A):-1 is A mod 2.
my_uppercase21(A):-upcase_atom(A,A).
my_pred22(A,B):-succ(B,A),A > 0.
my_len23(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_min_list4/2).
prim(my_list_to_set5/2).
prim(my_flatten6/2).
prim(my_succ7/2).
prim(my_even8/1).
prim(my_lowercase9/1).
prim(my_last10/2).
prim(my_toupper11/2).
prim(my_tolower13/2).
prim(my_sumlist14/2).
prim(my_set15/1).
prim(my_msort16/2).
prim(my_element17/2).
prim(my_double18/2).
prim(my_max_list19/2).
prim(my_odd20/1).
prim(my_uppercase21/1).
prim(my_pred22/2).
prim(my_len23/2).
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
p([['C','g','O'],['l','Y','Z','a'],['A','J','A']],[['C','g'],['l','Y','Z'],['A','J']]).
p([['x','E','r'],['I','X','X'],['A','u','e','L']],[['x','E'],['I','X'],['A','u','e']]).
p([['G','U','f','a'],['j','C','Z','o'],['Y','z','w','C'],['r','e','T']],[['G','U','f'],['j','C','Z'],['Y','z','w'],['r','e']]).
p([['w','w','P','I'],['d','U','Q']],[['w','w','P'],['d','U']]).
p([['W','q','k','f'],['Q','T','m','J']],[['W','q','k'],['Q','T','m']]).
q([['y','V','U','F'],['y','O','R'],['A','n','N','b'],['j','Z','w']],[['y','V','U','F'],['y','O','R'],['A','n','N'],['j','Z','w']]).
q([['c','V','l','Y'],['G','x','F'],['G','r','r'],['y','V','Z','w']],[['c','V','l'],['G','x','F'],['G','r','r'],['y','V','Z']]).
q([['b','p','V','u'],['o','r','p','Z'],['G','u','l'],['Z','u','z']],[['b','p','V'],['o','r','p','Z'],['G','u','l'],['Z','u','z']]).
q([['t','S','H'],['c','Y','u'],['J','D','f','D']],[['t','S'],['c','Y','u'],['J','D','f','D']]).
q([['u','d','s'],['J','T','F','x'],['p','G','O']],[['u','d','s'],['J','T','F'],['p','G','O']]).
