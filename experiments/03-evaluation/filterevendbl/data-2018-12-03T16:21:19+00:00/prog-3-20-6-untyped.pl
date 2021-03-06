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
my_len4(A,B):-length(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
my_flatten7(A,B):-flatten(A,B).
my_element8(A,B):-member(B,A).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_last10(A,B):-last(A,B).
my_min_list11(A,B):-min_list(A,B).
my_set12(A):-list_to_set(A,A).
my_msort13(A,B):-msort(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_tail15([_|TL],TL).
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
my_head17([H|_],H).
my_succ18(A,B):-succ(A,B),B =< 10.
my_lowercase19(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist20(A,B):-sumlist(A,B).
my_toupper21(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse22(A,B):-reverse(A,B).
my_max_list23(A,B):-max_list(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_len4/2).
prim(my_odd5/1).
prim(my_list_to_set6/2).
prim(my_flatten7/2).
prim(my_element8/2).
prim(my_uppercase9/1).
prim(my_last10/2).
prim(my_min_list11/2).
prim(my_set12/1).
prim(my_msort13/2).
prim(my_pred14/2).
prim(my_tail15/2).
prim(my_tolower16/2).
prim(my_head17/2).
prim(my_succ18/2).
prim(my_lowercase19/1).
prim(my_sumlist20/2).
prim(my_toupper21/2).
prim(my_reverse22/2).
prim(my_max_list23/2).
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
p([9,2,4,4],[4,8,8]).
p([1,0,7,3],[0]).
p([4,0,2,2,5,2,5,7],[8,0,4,4,4]).
p([4,7,1,9,3],[8]).
p([0,4,9,0],[0,8,0]).
q([1,7,1,2,0],[4,9,0]).
q([0,2,1,4,3,4,4,4,0],[0,8,4,8,8,8,8,0]).
q([2,2,0,3],[0,4,4,6]).
q([4,0,0,0,2,4,2,9,4],[0,8,0,0,4,1,4,8,8]).
q([0,1,7,9,7,0,0,7,7],[8,0,0,0]).
