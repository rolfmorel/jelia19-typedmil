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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_len3(A,B):-length(A,B).
my_even4(A):-0 is A mod 2.
my_odd5(A):-1 is A mod 2.
my_max_list6(A,B):-max_list(A,B).

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

my_set8(A):-list_to_set(A,A).
my_msort9(A,B):-msort(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_head11([H|_],H).
my_list_to_set12(A,B):-list_to_set(A,B).
my_last13(A,B):-last(A,B).
my_flatten14(A,B):-flatten(A,B).
my_min_list15(A,B):-min_list(A,B).
my_toupper16(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_uppercase2/1).
prim(my_len3/2).
prim(my_even4/1).
prim(my_odd5/1).
prim(my_max_list6/2).
prim(my_set8/1).
prim(my_msort9/2).
prim(my_double10/2).
prim(my_head11/2).
prim(my_list_to_set12/2).
prim(my_last13/2).
prim(my_flatten14/2).
prim(my_min_list15/2).
prim(my_toupper16/2).
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
p([[5,0,7],[3,2,2,4]],[[7,2,9],[5,4,4,6]]).
p([[6,5,0],[3,5,2],[5,0,3]],[[8,7,2],[5,7,4],[7,2,5]]).
p([[4,2,4],[4,2,5,5],[2,4,5,1],[3,0,0,4]],[[6,4,6],[6,4,7,7],[4,6,7,3],[5,2,2,6]]).
p([[0,4,3],[2,4,1]],[[2,6,5],[4,6,3]]).
p([[5,1,7,2],[4,5,0,7]],[[7,3,9,4],[6,7,2,9]]).
q([[5,4,6],[1,4,6,7],[1,0,4,2],[6,6,6,0]],[[5,4,6],[3,6,8,9],[1,0,4,2],[8,8,8,2]]).
q([[3,1,4],[5,0,7,4],[7,1,2,7]],[[5,3,6],[5,0,7,4],[9,3,4,9]]).
q([[6,2,2,1],[1,3,1]],[[8,4,4,3],[1,3,1]]).
q([[7,2,1],[1,7,6],[4,4,3,3],[1,7,3,6]],[[9,4,3],[1,7,6],[6,6,5,5],[3,9,5,8]]).
q([[5,4,4],[3,5,0]],[[5,4,4],[5,7,2]]).
