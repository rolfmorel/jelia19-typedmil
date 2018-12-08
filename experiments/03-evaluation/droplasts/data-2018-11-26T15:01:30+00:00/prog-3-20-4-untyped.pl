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

my_len3(A,B):-length(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_msort7(A,B):-msort(A,B).
my_element8(A,B):-member(B,A).
my_uppercase9(A):-upcase_atom(A,A).
my_flatten10(A,B):-flatten(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.

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

my_set13(A):-list_to_set(A,A).
my_sumlist14(A,B):-sumlist(A,B).
my_min_list15(A,B):-min_list(A,B).
my_toupper16(A,B):-upcase_atom(A,B).
my_max_list17(A,B):-max_list(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
my_odd19(A):-1 is A mod 2.
my_even20(A):-0 is A mod 2.
my_list_to_set21(A,B):-list_to_set(A,B).
my_tolower22(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_len3/2).
prim(my_head4/2).
prim(my_last5/2).
prim(my_lowercase6/1).
prim(my_msort7/2).
prim(my_element8/2).
prim(my_uppercase9/1).
prim(my_flatten10/2).
prim(my_succ11/2).
prim(my_set13/1).
prim(my_sumlist14/2).
prim(my_min_list15/2).
prim(my_toupper16/2).
prim(my_max_list17/2).
prim(my_pred18/2).
prim(my_odd19/1).
prim(my_even20/1).
prim(my_list_to_set21/2).
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
p([['K','k','l'],['E','B','C','I'],['V','f','z'],['m','x','P']],[['K','k'],['E','B','C'],['V','f'],['m','x']]).
p([['D','Y','p'],['i','s','R','r'],['z','a','v']],[['D','Y'],['i','s','R'],['z','a']]).
p([['u','w','g'],['d','c','A'],['y','B','q']],[['u','w'],['d','c'],['y','B']]).
p([['q','x','t','t'],['S','N','S','B']],[['q','x','t'],['S','N','S']]).
p([['G','s','n'],['G','a','C']],[['G','s'],['G','a']]).
q([['I','R','a'],['f','b','N'],['y','T','f'],['r','K','y','Q']],[['I','R'],['f','b'],['y','T','f'],['r','K','y','Q']]).
q([['u','r','D','b'],['G','f','c'],['m','o','W']],[['u','r','D','b'],['G','f'],['m','o','W']]).
q([['o','J','h','M'],['D','p','i'],['x','q','q'],['n','A','i']],[['o','J','h','M'],['D','p','i'],['x','q','q'],['n','A']]).
q([['L','L','y','n'],['I','M','K','k']],[['L','L','y','n'],['I','M','K']]).
q([['h','V','h'],['g','j','B'],['m','x','y','b']],[['h','V','h'],['g','j','B'],['m','x','y']]).
