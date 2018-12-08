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

my_lowercase3(A):-downcase_atom(A,A).
my_even4(A):-0 is A mod 2.
my_pred5(A,B):-succ(B,A),A > 0.
my_succ6(A,B):-succ(A,B),B =< 10.
my_list_to_set7(A,B):-list_to_set(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_element9(A,B):-member(B,A).
my_set10(A):-list_to_set(A,A).

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

my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_max_list14(A,B):-max_list(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_flatten16(A,B):-flatten(A,B).
my_last17(A,B):-last(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_odd19(A):-1 is A mod 2.
my_toupper20(A,B):-upcase_atom(A,B).
my_msort21(A,B):-msort(A,B).
my_tolower22(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_even4/1).
prim(my_pred5/2).
prim(my_succ6/2).
prim(my_list_to_set7/2).
prim(my_double8/2).
prim(my_element9/2).
prim(my_set10/1).
prim(my_head12/2).
prim(my_len13/2).
prim(my_max_list14/2).
prim(my_uppercase15/1).
prim(my_flatten16/2).
prim(my_last17/2).
prim(my_sumlist18/2).
prim(my_odd19/1).
prim(my_toupper20/2).
prim(my_msort21/2).
prim(my_tolower22/2).
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
p([['W','r','G'],['J','Q','M']],[['W','r'],['J','Q']]).
p([['I','v','l','m'],['G','U','h']],[['I','v','l'],['G','U']]).
p([['u','r','D','k'],['m','m','n','i'],['o','I','x']],[['u','r','D'],['m','m','n'],['o','I']]).
p([['w','z','Q'],['D','G','D','C']],[['w','z'],['D','G','D']]).
p([['T','v','A','c'],['t','t','Z','Y'],['z','g','W','X'],['v','f','H','r']],[['T','v','A'],['t','t','Z'],['z','g','W'],['v','f','H']]).
q([['F','m','x'],['G','i','g'],['a','B','Q','X'],['i','i','P']],[['F','m','x'],['G','i'],['a','B','Q','X'],['i','i']]).
q([['q','W','o','E'],['F','h','D'],['b','O','M','h'],['E','p','C']],[['q','W','o','E'],['F','h'],['b','O','M','h'],['E','p','C']]).
q([['z','l','s','I'],['Q','f','i']],[['z','l','s'],['Q','f','i']]).
q([['Y','A','p'],['Q','b','I','F']],[['Y','A','p'],['Q','b','I']]).
q([['U','T','a','u'],['I','R','n','O'],['c','K','C','n']],[['U','T','a','u'],['I','R','n'],['c','K','C','n']]).
