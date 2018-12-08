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

my_flatten3(A,B):-flatten(A,B).
my_odd4(A):-1 is A mod 2.
my_toupper5(A,B):-upcase_atom(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_sumlist8(A,B):-sumlist(A,B).

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

my_succ10(A,B):-succ(A,B),B =< 10.
my_even11(A):-0 is A mod 2.
my_max_list12(A,B):-max_list(A,B).
my_lowercase13(A):-downcase_atom(A,A).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_uppercase16(A):-upcase_atom(A,A).
my_element17(A,B):-member(B,A).
my_list_to_set18(A,B):-list_to_set(A,B).
my_pred19(A,B):-succ(B,A),A > 0.
my_msort20(A,B):-msort(A,B).
my_set21(A):-list_to_set(A,A).
my_min_list22(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_odd4/1).
prim(my_toupper5/2).
prim(my_tolower6/2).
prim(my_double7/2).
prim(my_sumlist8/2).
prim(my_succ10/2).
prim(my_even11/1).
prim(my_max_list12/2).
prim(my_lowercase13/1).
prim(my_len14/2).
prim(my_last15/2).
prim(my_uppercase16/1).
prim(my_element17/2).
prim(my_list_to_set18/2).
prim(my_pred19/2).
prim(my_msort20/2).
prim(my_set21/1).
prim(my_min_list22/2).
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
p([['V','j','T','h'],['C','c','X'],['O','U','a'],['q','t','P','e']],[['V','j','T'],['C','c'],['O','U'],['q','t','P']]).
p([['L','k','t'],['M','u','c'],['i','V','p'],['w','a','J']],[['L','k'],['M','u'],['i','V'],['w','a']]).
p([['r','K','H','M'],['v','V','k'],['x','L','j','f'],['U','f','k','t']],[['r','K','H'],['v','V'],['x','L','j'],['U','f','k']]).
p([['b','J','L'],['v','G','o','a']],[['b','J'],['v','G','o']]).
p([['Y','I','S','D'],['s','B','v']],[['Y','I','S'],['s','B']]).
q([['f','f','D'],['K','W','e'],['r','k','d']],[['f','f','D'],['K','W'],['r','k','d']]).
q([['V','U','k'],['A','H','D'],['k','d','B','o'],['P','R','o','b']],[['V','U','k'],['A','H','D'],['k','d','B'],['P','R','o','b']]).
q([['S','B','v','c'],['I','Z','O'],['J','M','H']],[['S','B','v','c'],['I','Z','O'],['J','M']]).
q([['w','k','C','n'],['s','o','k','C']],[['w','k','C'],['s','o','k','C']]).
q([['B','r','a'],['w','j','c'],['x','G','I','E']],[['B','r','a'],['w','j','c'],['x','G','I']]).
