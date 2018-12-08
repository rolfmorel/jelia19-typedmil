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
my_head4([H|_],H).
my_toupper5(A,B):-upcase_atom(A,B).
my_even6(A):-0 is A mod 2.
my_succ7(A,B):-succ(A,B),B =< 10.
my_flatten8(A,B):-flatten(A,B).
my_element9(A,B):-member(B,A).
my_len10(A,B):-length(A,B).
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).
my_last13(A,B):-last(A,B).

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

my_max_list15(A,B):-max_list(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
my_min_list17(A,B):-min_list(A,B).
my_uppercase18(A):-upcase_atom(A,A).
my_double19(N,M):-M is 2*N,M =< 10.
my_sumlist20(A,B):-sumlist(A,B).
my_odd21(A):-1 is A mod 2.
my_tolower22(A,B):-downcase_atom(A,B).
my_pred23(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_head4/2).
prim(my_toupper5/2).
prim(my_even6/1).
prim(my_succ7/2).
prim(my_flatten8/2).
prim(my_element9/2).
prim(my_len10/2).
prim(my_msort11/2).
prim(my_set12/1).
prim(my_last13/2).
prim(my_max_list15/2).
prim(my_list_to_set16/2).
prim(my_min_list17/2).
prim(my_uppercase18/1).
prim(my_double19/2).
prim(my_sumlist20/2).
prim(my_odd21/1).
prim(my_tolower22/2).
prim(my_pred23/2).
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
p([['j','S','r','I'],['E','E','C','q'],['R','e','w'],['L','R','R']],[['j','S','r'],['E','E','C'],['R','e'],['L','R']]).
p([['Q','a','Z'],['v','g','g']],[['Q','a'],['v','g']]).
p([['I','b','b'],['Z','r','N']],[['I','b'],['Z','r']]).
p([['X','x','T','x'],['q','j','b']],[['X','x','T'],['q','j']]).
p([['x','D','C'],['B','t','W']],[['x','D'],['B','t']]).
q([['f','H','L','Z'],['H','F','m','T'],['G','G','d','T']],[['f','H','L'],['H','F','m','T'],['G','G','d','T']]).
q([['D','v','Q','L'],['W','H','b','U'],['I','D','A','L']],[['D','v','Q'],['W','H','b','U'],['I','D','A','L']]).
q([['j','M','J','q'],['E','R','E','k'],['t','l','X'],['F','E','D']],[['j','M','J','q'],['E','R','E','k'],['t','l'],['F','E','D']]).
q([['o','O','S'],['u','n','o','o'],['m','w','o'],['E','d','G','S']],[['o','O'],['u','n','o','o'],['m','w'],['E','d','G','S']]).
q([['U','k','D'],['U','p','h','c'],['d','R','I'],['s','X','C','K']],[['U','k','D'],['U','p','h','c'],['d','R','I'],['s','X','C']]).
