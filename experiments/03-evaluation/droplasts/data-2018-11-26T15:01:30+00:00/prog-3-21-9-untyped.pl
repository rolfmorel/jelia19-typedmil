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

my_even3(A):-0 is A mod 2.
my_succ4(A,B):-succ(A,B),B =< 10.
my_list_to_set5(A,B):-list_to_set(A,B).
my_set6(A):-list_to_set(A,A).
my_element7(A,B):-member(B,A).
my_uppercase8(A):-upcase_atom(A,A).
my_head9([H|_],H).
my_min_list10(A,B):-min_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_flatten14(A,B):-flatten(A,B).
my_double15(N,M):-M is 2*N,M =< 10.

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

my_lowercase17(A):-downcase_atom(A,A).
my_len18(A,B):-length(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_msort20(A,B):-msort(A,B).
my_max_list21(A,B):-max_list(A,B).
my_last22(A,B):-last(A,B).
my_odd23(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_succ4/2).
prim(my_list_to_set5/2).
prim(my_set6/1).
prim(my_element7/2).
prim(my_uppercase8/1).
prim(my_head9/2).
prim(my_min_list10/2).
prim(my_tolower11/2).
prim(my_toupper12/2).
prim(my_pred13/2).
prim(my_flatten14/2).
prim(my_double15/2).
prim(my_lowercase17/1).
prim(my_len18/2).
prim(my_sumlist19/2).
prim(my_msort20/2).
prim(my_max_list21/2).
prim(my_last22/2).
prim(my_odd23/1).
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
p([['h','t','W','O'],['A','Y','o','o'],['b','K','n']],[['h','t','W'],['A','Y','o'],['b','K']]).
p([['z','a','a'],['L','A','R'],['a','K','g','U']],[['z','a'],['L','A'],['a','K','g']]).
p([['o','R','y'],['l','d','Y'],['f','F','N','D'],['Z','K','h']],[['o','R'],['l','d'],['f','F','N'],['Z','K']]).
p([['y','Q','Y','p'],['w','K','T']],[['y','Q','Y'],['w','K']]).
p([['R','c','s','i'],['e','c','a'],['O','v','A','m']],[['R','c','s'],['e','c'],['O','v','A']]).
q([['v','T','n','p'],['Z','g','c'],['T','T','j','c']],[['v','T','n','p'],['Z','g','c'],['T','T','j']]).
q([['N','s','J'],['k','B','W','r'],['F','k','I','f']],[['N','s','J'],['k','B','W','r'],['F','k','I']]).
q([['p','c','g','p'],['D','N','p'],['g','F','p'],['R','f','L']],[['p','c','g','p'],['D','N','p'],['g','F'],['R','f','L']]).
q([['i','g','C','S'],['D','o','v'],['Z','G','J','T'],['W','q','O']],[['i','g','C','S'],['D','o'],['Z','G','J','T'],['W','q','O']]).
q([['o','v','n','K'],['S','C','V','H'],['v','Z','h']],[['o','v','n','K'],['S','C','V','H'],['v','Z']]).
