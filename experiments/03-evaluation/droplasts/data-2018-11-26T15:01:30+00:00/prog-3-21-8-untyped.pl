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

my_last3(A,B):-last(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_max_list5(A,B):-max_list(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_lowercase7(A):-downcase_atom(A,A).
my_odd8(A):-1 is A mod 2.
my_len9(A,B):-length(A,B).
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
my_tolower13(A,B):-downcase_atom(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_flatten16(A,B):-flatten(A,B).
my_even17(A):-0 is A mod 2.
my_toupper18(A,B):-upcase_atom(A,B).
my_element19(A,B):-member(B,A).
my_msort20(A,B):-msort(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_pred22(A,B):-succ(B,A),A > 0.
my_min_list23(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
prim(my_uppercase4/1).
prim(my_max_list5/2).
prim(my_succ6/2).
prim(my_lowercase7/1).
prim(my_odd8/1).
prim(my_len9/2).
prim(my_set10/1).
prim(my_head12/2).
prim(my_tolower13/2).
prim(my_list_to_set14/2).
prim(my_double15/2).
prim(my_flatten16/2).
prim(my_even17/1).
prim(my_toupper18/2).
prim(my_element19/2).
prim(my_msort20/2).
prim(my_sumlist21/2).
prim(my_pred22/2).
prim(my_min_list23/2).
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
p([['P','l','a'],['j','E','Q','j'],['n','V','j','v'],['i','V','R','V']],[['P','l'],['j','E','Q'],['n','V','j'],['i','V','R']]).
p([['B','v','r'],['U','v','p','A']],[['B','v'],['U','v','p']]).
p([['A','e','K'],['G','d','C'],['M','O','b'],['s','O','o']],[['A','e'],['G','d'],['M','O'],['s','O']]).
p([['c','b','U','e'],['S','s','K','W'],['i','f','D','l']],[['c','b','U'],['S','s','K'],['i','f','D']]).
p([['a','X','X'],['x','Y','s','e'],['f','M','d'],['v','o','G','A']],[['a','X'],['x','Y','s'],['f','M'],['v','o','G']]).
q([['q','s','c'],['L','O','D'],['r','B','F'],['b','s','c','t']],[['q','s'],['L','O','D'],['r','B','F'],['b','s','c','t']]).
q([['f','q','j','R'],['f','g','b','r'],['e','U','P'],['Q','q','A']],[['f','q','j','R'],['f','g','b'],['e','U','P'],['Q','q','A']]).
q([['i','D','a'],['l','k','x'],['r','T','d'],['T','D','V']],[['i','D','a'],['l','k'],['r','T'],['T','D','V']]).
q([['k','n','x'],['T','l','z','P'],['Q','Q','D']],[['k','n','x'],['T','l','z','P'],['Q','Q']]).
q([['C','W','s','W'],['m','i','f','K']],[['C','W','s'],['m','i','f','K']]).
