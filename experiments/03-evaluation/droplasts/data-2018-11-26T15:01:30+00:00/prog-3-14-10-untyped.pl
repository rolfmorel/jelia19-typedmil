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
my_msort4(A,B):-msort(A,B).
my_set5(A):-list_to_set(A,A).
my_even6(A):-0 is A mod 2.
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_flatten9(A,B):-flatten(A,B).
my_last10(A,B):-last(A,B).
my_element11(A,B):-member(B,A).
my_succ12(A,B):-succ(A,B),B =< 10.
my_odd13(A):-1 is A mod 2.
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B).

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

prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_msort4/2).
prim(my_set5/1).
prim(my_even6/1).
prim(my_head7/2).
prim(my_min_list8/2).
prim(my_flatten9/2).
prim(my_last10/2).
prim(my_element11/2).
prim(my_succ12/2).
prim(my_odd13/1).
prim(my_list_to_set14/2).
prim(my_tolower15/2).
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
p([['q','y','f'],['K','e','N','z'],['D','m','f','u'],['u','Z','n','V']],[['q','y'],['K','e','N'],['D','m','f'],['u','Z','n']]).
p([['s','c','h'],['h','w','m'],['P','G','t','r'],['k','E','H']],[['s','c'],['h','w'],['P','G','t'],['k','E']]).
p([['V','a','l'],['G','K','b'],['z','I','I']],[['V','a'],['G','K'],['z','I']]).
p([['X','M','x','z'],['O','C','T','B'],['J','g','L','L'],['w','O','X','I']],[['X','M','x'],['O','C','T'],['J','g','L'],['w','O','X']]).
p([['n','W','Z'],['p','P','C']],[['n','W'],['p','P']]).
q([['W','E','A'],['X','r','C'],['H','A','x','i']],[['W','E','A'],['X','r','C'],['H','A','x']]).
q([['p','V','Z','W'],['T','x','U'],['y','H','D','w']],[['p','V','Z'],['T','x','U'],['y','H','D','w']]).
q([['H','L','Y'],['R','D','G','K'],['w','m','e','f'],['P','T','P']],[['H','L'],['R','D','G','K'],['w','m','e'],['P','T','P']]).
q([['j','U','u','k'],['O','c','f'],['R','R','H'],['z','O','C']],[['j','U','u','k'],['O','c'],['R','R','H'],['z','O','C']]).
q([['e','T','i','p'],['e','p','J']],[['e','T','i'],['e','p','J']]).
