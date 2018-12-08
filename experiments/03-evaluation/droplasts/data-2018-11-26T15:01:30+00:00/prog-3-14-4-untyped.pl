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

my_element3(A,B):-member(B,A).

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

my_set5(A):-list_to_set(A,A).
my_msort6(A,B):-msort(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_sumlist8(A,B):-sumlist(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_pred10(A,B):-succ(B,A),A > 0.
my_flatten11(A,B):-flatten(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_uppercase16(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_element3/2).
prim(my_set5/1).
prim(my_msort6/2).
prim(my_lowercase7/1).
prim(my_sumlist8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_flatten11/2).
prim(my_double12/2).
prim(my_min_list13/2).
prim(my_len14/2).
prim(my_last15/2).
prim(my_uppercase16/1).
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
p([['w','l','p'],['u','B','E','R'],['c','s','I','H']],[['w','l'],['u','B','E'],['c','s','I']]).
p([['R','P','D','d'],['k','L','v','C'],['u','t','z']],[['R','P','D'],['k','L','v'],['u','t']]).
p([['g','W','D'],['n','E','R','n'],['C','B','h','r'],['e','m','V']],[['g','W'],['n','E','R'],['C','B','h'],['e','m']]).
p([['n','W','q'],['L','l','B'],['W','r','V','C']],[['n','W'],['L','l'],['W','r','V']]).
p([['Y','R','P'],['s','O','H'],['S','I','h']],[['Y','R'],['s','O'],['S','I']]).
q([['r','r','G'],['l','r','M','H'],['T','a','f','x']],[['r','r','G'],['l','r','M'],['T','a','f','x']]).
q([['w','f','N'],['i','e','b'],['x','k','k']],[['w','f','N'],['i','e'],['x','k','k']]).
q([['h','A','v','R'],['J','f','G']],[['h','A','v'],['J','f','G']]).
q([['Q','l','d'],['e','f','g'],['F','e','S','v']],[['Q','l','d'],['e','f'],['F','e','S','v']]).
q([['l','C','U','j'],['Y','z','E'],['f','A','z'],['e','V','L','a']],[['l','C','U'],['Y','z','E'],['f','A','z'],['e','V','L','a']]).
