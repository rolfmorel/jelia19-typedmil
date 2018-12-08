:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_even2(A):-0 is A mod 2.
my_double3(N,M):-M is 2*N,M =< 10.
my_reverse4(A,B):-reverse(A,B).
my_flatten5(A,B):-flatten(A,B).
my_tail6([_|TL],TL).
my_odd7(A):-1 is A mod 2.
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_element9(A,B):-member(B,A).
my_min_list10(A,B):-min_list(A,B).
my_msort11(A,B):-msort(A,B).
my_uppercase12(A):-upcase_atom(A,A),char_code(A,_).
my_max_list13(A,B):-max_list(A,B).
my_head14([H|_],H).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
my_succ17(A,B):-succ(A,B),B =< 10.
my_list_to_set18(A,B):-list_to_set(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_len20(A,B):-length(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_reverse4/2).
prim(my_flatten5/2).
prim(my_tail6/2).
prim(my_odd7/1).
prim(my_toupper8/2).
prim(my_element9/2).
prim(my_min_list10/2).
prim(my_msort11/2).
prim(my_uppercase12/1).
prim(my_max_list13/2).
prim(my_head14/2).
prim(my_tolower15/2).
prim(my_lowercase16/1).
prim(my_succ17/2).
prim(my_list_to_set18/2).
prim(my_sumlist19/2).
prim(my_len20/2).
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
p([2,0,5,7,7,9,4,5,4],[4,0,8,8]).
p([9,2,7,0,2,0,4],[4,0,4,0,8]).
p([3,1,5,4,2,2,9,2,9],[8,4,4,4]).
p([0,0,4,9,4],[0,0,8,8]).
p([2,0,3,2,4,1,5,0,9],[4,0,4,8,0]).
q([2,7,9,2,9,2],[8,4,4,4]).
q([3,2,3,9,5,4,2,9,9],[4,4,5,8]).
q([9,2,1,3],[4,4]).
q([0,2,4,0,3],[0,0,8,8,4]).
q([1,5,4,9],[4,8]).
