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

my_uppercase3(A):-upcase_atom(A,A).
my_last4(A,B):-last(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).

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

my_toupper14(A,B):-upcase_atom(A,B).
my_element15(A,B):-member(B,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_uppercase3/1).
prim(my_last4/2).
prim(my_tolower5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_lowercase8/1).
prim(my_list_to_set9/2).
prim(my_even10/1).
prim(my_msort11/2).
prim(my_set12/1).
prim(my_toupper14/2).
prim(my_element15/2).
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
p([['H','S','h'],['m','s','R']],[['H','S'],['m','s']]).
p([['c','z','m'],['v','q','U','Y']],[['c','z'],['v','q','U']]).
p([['q','n','v'],['u','v','w','u'],['s','I','J','W']],[['q','n'],['u','v','w'],['s','I','J']]).
p([['J','T','p'],['n','v','i']],[['J','T'],['n','v']]).
p([['j','B','K'],['q','w','x','s'],['g','X','Y','W']],[['j','B'],['q','w','x'],['g','X','Y']]).
q([['X','D','S','M'],['U','U','t']],[['X','D','S','M'],['U','U']]).
q([['H','Q','u','q'],['a','T','G','H'],['j','P','B'],['u','x','U','d']],[['H','Q','u'],['a','T','G','H'],['j','P'],['u','x','U','d']]).
q([['J','T','I','V'],['i','C','R'],['z','x','h'],['o','N','o']],[['J','T','I','V'],['i','C'],['z','x'],['o','N','o']]).
q([['F','s','B'],['W','D','P']],[['F','s'],['W','D','P']]).
q([['S','Q','e'],['a','I','z','w'],['W','E','v','G']],[['S','Q','e'],['a','I','z'],['W','E','v','G']]).
