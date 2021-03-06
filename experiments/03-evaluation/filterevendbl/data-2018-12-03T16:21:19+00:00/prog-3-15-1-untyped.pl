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
my_tail4([_|TL],TL).
my_flatten5(A,B):-flatten(A,B).
my_max_list6(A,B):-max_list(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_toupper9(A,B):-upcase_atom(A,B),char_code(A,_).
my_set10(A):-list_to_set(A,A).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_head14([H|_],H).
my_min_list15(A,B):-min_list(A,B).
my_element16(A,B):-member(B,A).
my_odd17(A):-1 is A mod 2.
my_reverse18(A,B):-reverse(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_tail4/2).
prim(my_flatten5/2).
prim(my_max_list6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_toupper9/2).
prim(my_set10/1).
prim(my_uppercase11/1).
prim(my_tolower12/2).
prim(my_lowercase13/1).
prim(my_head14/2).
prim(my_min_list15/2).
prim(my_element16/2).
prim(my_odd17/1).
prim(my_reverse18/2).
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
p([4,2,1,0],[8,4,0]).
p([3,4,7,2,5,2,2,9],[8,4,4,4]).
p([5,4,2,2,2,4,7],[8,4,4,4,8]).
p([4,2,0,2,5,7,4],[8,4,0,4,8]).
p([4,9,4,0,0,5],[8,8,0,0]).
q([2,2,7,2,5,0,2],[5,4,4,4,0,4]).
q([9,2,3,4,0],[0,4,8,4]).
q([5,4,2,3],[4,2,8]).
q([0,2,0,2,7,2,4,3],[4,4,8,0,0,5,4]).
q([3,0,5,4,2,5,3],[8,0,4,4]).
