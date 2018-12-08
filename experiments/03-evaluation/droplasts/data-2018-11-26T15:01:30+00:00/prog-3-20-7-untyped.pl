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

my_msort3(A,B):-msort(A,B).
my_odd4(A):-1 is A mod 2.
my_tolower5(A,B):-downcase_atom(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_max_list8(A,B):-max_list(A,B).

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

my_element10(A,B):-member(B,A).
my_last11(A,B):-last(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_list_to_set13(A,B):-list_to_set(A,B).
my_min_list14(A,B):-min_list(A,B).
my_len15(A,B):-length(A,B).
my_head16([H|_],H).
my_set17(A):-list_to_set(A,A).
my_uppercase18(A):-upcase_atom(A,A).
my_flatten19(A,B):-flatten(A,B).
my_succ20(A,B):-succ(A,B),B =< 10.
my_even21(A):-0 is A mod 2.
my_toupper22(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_odd4/1).
prim(my_tolower5/2).
prim(my_sumlist6/2).
prim(my_lowercase7/1).
prim(my_max_list8/2).
prim(my_element10/2).
prim(my_last11/2).
prim(my_pred12/2).
prim(my_list_to_set13/2).
prim(my_min_list14/2).
prim(my_len15/2).
prim(my_head16/2).
prim(my_set17/1).
prim(my_uppercase18/1).
prim(my_flatten19/2).
prim(my_succ20/2).
prim(my_even21/1).
prim(my_toupper22/2).
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
p([['T','e','D'],['h','z','t'],['n','D','y'],['g','i','f']],[['T','e'],['h','z'],['n','D'],['g','i']]).
p([['C','z','U'],['T','O','Y'],['J','l','R','o'],['J','Q','A']],[['C','z'],['T','O'],['J','l','R'],['J','Q']]).
p([['U','A','A'],['y','K','Y'],['P','p','p']],[['U','A'],['y','K'],['P','p']]).
p([['m','B','x'],['J','q','F','H'],['h','Y','E'],['t','J','K','p']],[['m','B'],['J','q','F'],['h','Y'],['t','J','K']]).
p([['o','N','D'],['U','E','B','C'],['j','M','S'],['L','u','N','f']],[['o','N'],['U','E','B'],['j','M'],['L','u','N']]).
q([['M','I','f'],['w','f','c'],['W','t','A','y']],[['M','I'],['w','f','c'],['W','t','A','y']]).
q([['e','z','F'],['r','I','R','T'],['h','O','z','o'],['S','v','W']],[['e','z','F'],['r','I','R'],['h','O','z'],['S','v','W']]).
q([['M','K','T'],['p','T','d','A'],['K','O','V','h']],[['M','K','T'],['p','T','d','A'],['K','O','V']]).
q([['R','q','S','d'],['v','u','r','r'],['Q','t','y']],[['R','q','S','d'],['v','u','r'],['Q','t','y']]).
q([['C','b','X','a'],['J','M','d']],[['C','b','X','a'],['J','M']]).
