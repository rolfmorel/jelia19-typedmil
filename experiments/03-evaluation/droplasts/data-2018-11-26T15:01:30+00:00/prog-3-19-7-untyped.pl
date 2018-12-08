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

my_sumlist3(A,B):-sumlist(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_set5(A):-list_to_set(A,A).
my_lowercase6(A):-downcase_atom(A,A).

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

my_pred8(A,B):-succ(B,A),A > 0.
my_odd9(A):-1 is A mod 2.
my_double10(N,M):-M is 2*N,M =< 10.
my_head11([H|_],H).
my_msort12(A,B):-msort(A,B).
my_succ13(A,B):-succ(A,B),B =< 10.
my_max_list14(A,B):-max_list(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_tolower16(A,B):-downcase_atom(A,B).
my_min_list17(A,B):-min_list(A,B).
my_element18(A,B):-member(B,A).
my_flatten19(A,B):-flatten(A,B).
my_even20(A):-0 is A mod 2.
my_len21(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_sumlist3/2).
prim(my_uppercase4/1).
prim(my_set5/1).
prim(my_lowercase6/1).
prim(my_pred8/2).
prim(my_odd9/1).
prim(my_double10/2).
prim(my_head11/2).
prim(my_msort12/2).
prim(my_succ13/2).
prim(my_max_list14/2).
prim(my_toupper15/2).
prim(my_tolower16/2).
prim(my_min_list17/2).
prim(my_element18/2).
prim(my_flatten19/2).
prim(my_even20/1).
prim(my_len21/2).
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
p([['m','r','T'],['d','Z','G'],['D','c','j'],['L','k','T']],[['m','r'],['d','Z'],['D','c'],['L','k']]).
p([['l','A','c'],['w','f','Y'],['O','P','m','h'],['e','m','W']],[['l','A'],['w','f'],['O','P','m'],['e','m']]).
p([['V','C','R','b'],['o','I','D'],['A','t','y','x']],[['V','C','R'],['o','I'],['A','t','y']]).
p([['w','M','n'],['A','Z','t'],['R','J','x','L'],['F','l','E','N']],[['w','M'],['A','Z'],['R','J','x'],['F','l','E']]).
p([['h','d','n'],['T','o','B'],['d','j','h']],[['h','d'],['T','o'],['d','j']]).
q([['V','a','R'],['C','G','k','W'],['V','O','y'],['P','z','s','k']],[['V','a'],['C','G','k'],['V','O','y'],['P','z','s','k']]).
q([['z','D','F','s'],['m','L','i'],['i','V','D']],[['z','D','F','s'],['m','L','i'],['i','V']]).
q([['k','a','w','W'],['Y','U','Z'],['V','I','G','f']],[['k','a','w','W'],['Y','U'],['V','I','G','f']]).
q([['q','l','r'],['f','G','Q','M'],['Z','O','W'],['c','T','u','u']],[['q','l'],['f','G','Q'],['Z','O','W'],['c','T','u','u']]).
q([['c','e','U','A'],['g','r','F'],['j','m','s','V']],[['c','e','U'],['g','r','F'],['j','m','s','V']]).
