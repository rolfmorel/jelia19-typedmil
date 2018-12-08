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
my_tolower4(A,B):-downcase_atom(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_head6([H|_],H).
my_flatten7(A,B):-flatten(A,B).
my_min_list8(A,B):-min_list(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_msort10(A,B):-msort(A,B).

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

my_len12(A,B):-length(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_odd14(A):-1 is A mod 2.
my_lowercase15(A):-downcase_atom(A,A).
my_sumlist16(A,B):-sumlist(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_element18(A,B):-member(B,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
prim(my_tolower4/2).
prim(my_toupper5/2).
prim(my_head6/2).
prim(my_flatten7/2).
prim(my_min_list8/2).
prim(my_double9/2).
prim(my_msort10/2).
prim(my_len12/2).
prim(my_uppercase13/1).
prim(my_odd14/1).
prim(my_lowercase15/1).
prim(my_sumlist16/2).
prim(my_pred17/2).
prim(my_element18/2).
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
p([['E','o','u'],['G','V','J'],['Z','h','R'],['f','t','H','B']],[['E','o'],['G','V'],['Z','h'],['f','t','H']]).
p([['Q','q','z','e'],['G','V','a'],['k','h','U'],['L','F','m']],[['Q','q','z'],['G','V'],['k','h'],['L','F']]).
p([['d','Z','p','d'],['C','I','N','U']],[['d','Z','p'],['C','I','N']]).
p([['u','C','M','f'],['L','V','p','T']],[['u','C','M'],['L','V','p']]).
p([['W','p','F'],['q','L','s']],[['W','p'],['q','L']]).
q([['w','y','O','o'],['s','m','c'],['y','U','X','w'],['p','E','T','P']],[['w','y','O','o'],['s','m','c'],['y','U','X'],['p','E','T']]).
q([['J','x','O'],['J','W','b','r'],['r','N','m','h'],['J','E','F']],[['J','x','O'],['J','W','b'],['r','N','m','h'],['J','E']]).
q([['f','M','Z','P'],['D','M','t','R']],[['f','M','Z','P'],['D','M','t']]).
q([['r','p','b'],['e','u','V','V'],['U','K','e']],[['r','p','b'],['e','u','V','V'],['U','K']]).
q([['j','y','P','c'],['U','M','o'],['y','P','r','e']],[['j','y','P','c'],['U','M'],['y','P','r','e']]).
