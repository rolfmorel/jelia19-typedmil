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

my_toupper4(A,B):-upcase_atom(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_tolower6(A,B):-downcase_atom(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_even11(A):-0 is A mod 2.
my_set12(A):-list_to_set(A,A).
my_lowercase13(A):-downcase_atom(A,A).
my_msort14(A,B):-msort(A,B).
my_element15(A,B):-member(B,A).
my_uppercase16(A):-upcase_atom(A,A).
my_flatten17(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_toupper4/2).
prim(my_double5/2).
prim(my_tolower6/2).
prim(my_succ7/2).
prim(my_last8/2).
prim(my_sumlist9/2).
prim(my_pred10/2).
prim(my_even11/1).
prim(my_set12/1).
prim(my_lowercase13/1).
prim(my_msort14/2).
prim(my_element15/2).
prim(my_uppercase16/1).
prim(my_flatten17/2).
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
p([['A','T','i'],['o','R','c'],['O','S','Z','N']],[['A','T'],['o','R'],['O','S','Z']]).
p([['C','c','j','E'],['A','h','h','E'],['K','S','g']],[['C','c','j'],['A','h','h'],['K','S']]).
p([['K','H','B','X'],['T','o','T']],[['K','H','B'],['T','o']]).
p([['l','L','H'],['y','a','l','n']],[['l','L'],['y','a','l']]).
p([['O','V','c','W'],['e','c','K','d'],['l','r','h','i']],[['O','V','c'],['e','c','K'],['l','r','h']]).
q([['i','p','t'],['p','l','X'],['e','M','L','W'],['W','L','q']],[['i','p'],['p','l','X'],['e','M','L','W'],['W','L','q']]).
q([['s','g','r'],['Z','U','g']],[['s','g'],['Z','U','g']]).
q([['I','n','a','C'],['q','b','r','G'],['X','b','q','B'],['f','m','b']],[['I','n','a','C'],['q','b','r'],['X','b','q','B'],['f','m','b']]).
q([['W','o','m','F'],['x','S','g','i']],[['W','o','m','F'],['x','S','g']]).
q([['U','u','Z','S'],['g','k','c','U']],[['U','u','Z','S'],['g','k','c']]).
