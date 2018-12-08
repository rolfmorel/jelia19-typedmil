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

my_pred3(A,B):-succ(B,A),A > 0.
my_set4(A):-list_to_set(A,A).

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

my_head6([H|_],H).
my_element7(A,B):-member(B,A).
my_lowercase8(A):-downcase_atom(A,A).
my_odd9(A):-1 is A mod 2.
my_toupper10(A,B):-upcase_atom(A,B).
my_msort11(A,B):-msort(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_last13(A,B):-last(A,B).
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B),B =< 10.
my_flatten17(A,B):-flatten(A,B).
my_even18(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred3/2).
prim(my_set4/1).
prim(my_head6/2).
prim(my_element7/2).
prim(my_lowercase8/1).
prim(my_odd9/1).
prim(my_toupper10/2).
prim(my_msort11/2).
prim(my_list_to_set12/2).
prim(my_last13/2).
prim(my_min_list14/2).
prim(my_sumlist15/2).
prim(my_succ16/2).
prim(my_flatten17/2).
prim(my_even18/1).
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
p([['l','g','q'],['g','b','i','f']],[['l','g'],['g','b','i']]).
p([['n','s','s'],['z','r','V','h'],['R','r','T','y'],['r','R','S']],[['n','s'],['z','r','V'],['R','r','T'],['r','R']]).
p([['c','n','F','H'],['r','T','w']],[['c','n','F'],['r','T']]).
p([['T','O','U'],['M','M','w'],['F','p','R','j']],[['T','O'],['M','M'],['F','p','R']]).
p([['A','g','w'],['v','f','j'],['l','n','q'],['e','G','S','c']],[['A','g'],['v','f'],['l','n'],['e','G','S']]).
q([['p','T','Q','B'],['t','R','H','o'],['X','J','E'],['W','U','z']],[['p','T','Q','B'],['t','R','H'],['X','J'],['W','U','z']]).
q([['K','H','g','R'],['c','T','U'],['E','q','z','o'],['o','f','h']],[['K','H','g'],['c','T','U'],['E','q','z','o'],['o','f']]).
q([['S','D','y','s'],['u','T','X','I'],['W','w','e','r']],[['S','D','y','s'],['u','T','X','I'],['W','w','e']]).
q([['O','A','e'],['A','t','t'],['x','e','E','n'],['u','Z','y']],[['O','A','e'],['A','t'],['x','e','E'],['u','Z','y']]).
q([['n','z','L','v'],['d','q','R'],['b','g','m','Q'],['i','z','w','F']],[['n','z','L','v'],['d','q','R'],['b','g','m','Q'],['i','z','w']]).
