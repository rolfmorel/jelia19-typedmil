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

my_max_list3(A,B):-max_list(A,B).
my_flatten4(A,B):-flatten(A,B).
my_msort5(A,B):-msort(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_min_list7(A,B):-min_list(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_head9([H|_],H).
my_len10(A,B):-length(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_succ13(A,B):-succ(A,B),B =< 10.
my_double14(N,M):-M is 2*N,M =< 10.
my_lowercase15(A):-downcase_atom(A,A).

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

my_last17(A,B):-last(A,B).
my_tolower18(A,B):-downcase_atom(A,B).
my_element19(A,B):-member(B,A).
my_odd20(A):-1 is A mod 2.
my_even21(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_flatten4/2).
prim(my_msort5/2).
prim(my_sumlist6/2).
prim(my_min_list7/2).
prim(my_uppercase8/1).
prim(my_head9/2).
prim(my_len10/2).
prim(my_list_to_set11/2).
prim(my_pred12/2).
prim(my_succ13/2).
prim(my_double14/2).
prim(my_lowercase15/1).
prim(my_last17/2).
prim(my_tolower18/2).
prim(my_element19/2).
prim(my_odd20/1).
prim(my_even21/1).
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
p([['P','R','V'],['e','L','g'],['I','j','h','R'],['B','L','X','G']],[['P','R'],['e','L'],['I','j','h'],['B','L','X']]).
p([['m','D','S','k'],['m','U','Z'],['V','P','P','I']],[['m','D','S'],['m','U'],['V','P','P']]).
p([['l','k','c'],['m','K','A','o']],[['l','k'],['m','K','A']]).
p([['j','J','X'],['z','X','t'],['g','w','L','i']],[['j','J'],['z','X'],['g','w','L']]).
p([['C','C','B'],['k','y','j','H'],['O','F','N'],['n','U','r']],[['C','C'],['k','y','j'],['O','F'],['n','U']]).
q([['X','g','t'],['w','q','Q','H'],['w','H','H']],[['X','g','t'],['w','q','Q','H'],['w','H']]).
q([['G','v','P','Y'],['N','T','o'],['E','R','f','K']],[['G','v','P'],['N','T','o'],['E','R','f','K']]).
q([['R','E','a'],['O','s','E','n'],['D','E','D']],[['R','E','a'],['O','s','E'],['D','E','D']]).
q([['e','C','E'],['f','V','B'],['t','h','d'],['j','R','J']],[['e','C'],['f','V','B'],['t','h','d'],['j','R']]).
q([['w','d','E'],['K','h','D','R'],['o','S','C']],[['w','d','E'],['K','h','D','R'],['o','S']]).
