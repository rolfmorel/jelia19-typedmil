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
my_pred3(A,B):-succ(B,A),A > 0.
my_even4(A):-0 is A mod 2.
my_max_list5(A,B):-max_list(A,B).
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
my_double7(N,M):-M is 2*N,M =< 10.

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

prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_pred3/2).
prim(my_even4/1).
prim(my_max_list5/2).
prim(my_tolower6/2).
prim(my_double7/2).
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
p([[6,6,5,0],[3,5,6,4]],[[8,8,7,2],[5,7,8,6]]).
p([[4,0,5,2],[5,5,3,3],[3,1,7,3]],[[6,2,7,4],[7,7,5,5],[5,3,9,5]]).
p([[1,3,4],[3,0,2],[6,1,4,7]],[[3,5,6],[5,2,4],[8,3,6,9]]).
p([[4,7,4],[1,7,6,6],[2,6,6]],[[6,9,6],[3,9,8,8],[4,8,8]]).
p([[2,2,2,5],[3,1,0,7]],[[4,4,4,7],[5,3,2,9]]).
q([[5,7,1,5],[7,3,1,3],[5,3,7,5],[6,3,3,7]],[[5,7,1,5],[9,5,3,5],[7,5,9,7],[8,5,5,9]]).
q([[2,0,0],[1,2,3]],[[2,0,0],[3,4,5]]).
q([[0,1,5,0],[0,3,4,0],[3,5,1],[0,5,5]],[[2,3,7,2],[2,5,6,2],[3,5,1],[2,7,7]]).
q([[3,7,6,5],[2,5,6,3],[5,1,1,6],[2,3,0]],[[3,7,6,5],[2,5,6,3],[7,3,3,8],[4,5,2]]).
q([[6,1,1],[3,0,5],[5,7,4]],[[8,3,3],[3,0,5],[7,9,6]]).
