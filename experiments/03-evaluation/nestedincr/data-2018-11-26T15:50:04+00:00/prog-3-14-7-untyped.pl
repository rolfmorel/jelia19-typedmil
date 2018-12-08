:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_reverse2(A,B):-reverse(A,B).
my_odd3(A):-1 is A mod 2.
my_msort4(A,B):-msort(A,B).
my_head5([H|_],H).
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_even7(A):-0 is A mod 2.

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

my_double9(N,M):-M is 2*N,M =< 10.
my_set10(A):-list_to_set(A,A).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_element15(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_odd3/1).
prim(my_msort4/2).
prim(my_head5/2).
prim(my_lowercase6/1).
prim(my_even7/1).
prim(my_double9/2).
prim(my_set10/1).
prim(my_tail11/2).
prim(my_max_list12/2).
prim(my_len13/2).
prim(my_list_to_set14/2).
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
p([[2,3,7],[5,6,1,4],[1,4,0],[4,1,6]],[[4,5,9],[7,8,3,6],[3,6,2],[6,3,8]]).
p([[5,5,1],[0,2,7,0]],[[7,7,3],[2,4,9,2]]).
p([[0,6,5],[5,0,7,4],[2,6,1]],[[2,8,7],[7,2,9,6],[4,8,3]]).
p([[0,0,5,0],[4,5,2]],[[2,2,7,2],[6,7,4]]).
p([[0,0,7,2],[2,3,6,6]],[[2,2,9,4],[4,5,8,8]]).
q([[3,4,4,4],[0,7,4,0],[7,6,7],[3,5,2,7]],[[5,6,6,6],[0,7,4,0],[9,8,9],[5,7,4,9]]).
q([[2,5,3],[2,4,2]],[[2,5,3],[4,6,4]]).
q([[6,5,2,2],[3,5,6,6],[4,4,6],[1,4,6,7]],[[8,7,4,4],[5,7,8,8],[4,4,6],[3,6,8,9]]).
q([[5,0,1],[3,7,5],[3,7,5,5],[3,5,5,4]],[[7,2,3],[3,7,5],[5,9,7,7],[5,7,7,6]]).
q([[1,5,1,1],[0,5,7,6],[1,6,3,5],[0,1,6,6]],[[3,7,3,3],[0,5,7,6],[1,6,3,5],[2,3,8,8]]).
