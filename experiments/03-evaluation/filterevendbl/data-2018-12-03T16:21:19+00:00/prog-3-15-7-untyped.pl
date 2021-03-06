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
my_element4(A,B):-member(B,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_msort7(A,B):-msort(A,B).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_succ9(A,B):-succ(A,B),B =< 10.
my_last10(A,B):-last(A,B).
my_set11(A):-list_to_set(A,A).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist13(A,B):-sumlist(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_reverse15(A,B):-reverse(A,B).
my_odd16(A):-1 is A mod 2.
my_len17(A,B):-length(A,B).
my_head18([H|_],H).
prim(my_even2/1).
prim(my_double3/2).
prim(my_element4/2).
prim(my_pred5/2).
prim(my_uppercase6/1).
prim(my_msort7/2).
prim(my_tolower8/2).
prim(my_succ9/2).
prim(my_last10/2).
prim(my_set11/1).
prim(my_toupper12/2).
prim(my_sumlist13/2).
prim(my_list_to_set14/2).
prim(my_reverse15/2).
prim(my_odd16/1).
prim(my_len17/2).
prim(my_head18/2).
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
p([1,7,1,3,7,0,1],[0]).
p([7,4,7,4,2,7,7,5],[8,8,4]).
p([4,9,7,4,9,1,2,5],[8,8,4]).
p([0,0,9,5,1,7],[0,0]).
p([3,1,2,0,1,2,3,3],[4,0,4]).
q([1,9,5,7,7,0,4,4,9],[5,8,8,0]).
q([1,2,4,5,9,2,9],[4,8,4,8]).
q([4,5,9,5,1,9,2,3,3],[4,3,8]).
q([7,3,5,0,4,4,3],[6,8,8,0]).
q([4,1,0,4,4,5,4,5],[7,8,8,8,0,8]).
