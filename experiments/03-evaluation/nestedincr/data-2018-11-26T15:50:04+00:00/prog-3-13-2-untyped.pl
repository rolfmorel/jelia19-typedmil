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
my_last2(A,B):-last(A,B).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_head5([H|_],H).
my_msort6(A,B):-msort(A,B).
my_len7(A,B):-length(A,B).
my_set8(A):-list_to_set(A,A).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set11(A,B):-list_to_set(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_even13(A):-0 is A mod 2.
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_lowercase3/1).
prim(my_double4/2).
prim(my_head5/2).
prim(my_msort6/2).
prim(my_len7/2).
prim(my_set8/1).
prim(my_tolower9/2).
prim(my_uppercase10/1).
prim(my_list_to_set11/2).
prim(my_pred12/2).
prim(my_even13/1).
prim(my_toupper14/2).
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
p([[1,1,5],[4,5,2,4],[2,4,0,5]],[[3,3,7],[6,7,4,6],[4,6,2,7]]).
p([[2,3,5,0],[4,5,1],[6,7,6]],[[4,5,7,2],[6,7,3],[8,9,8]]).
p([[6,6,0,5],[2,6,3,0],[0,1,4],[5,0,4,5]],[[8,8,2,7],[4,8,5,2],[2,3,6],[7,2,6,7]]).
p([[6,7,5,2],[6,5,4,7]],[[8,9,7,4],[8,7,6,9]]).
p([[5,1,7],[3,4,1,4]],[[7,3,9],[5,6,3,6]]).
q([[3,7,1],[5,1,7],[2,0,3]],[[5,9,3],[7,3,9],[2,0,3]]).
q([[2,4,2,3],[3,1,4,1],[2,2,1,1],[6,1,3,4]],[[4,6,4,5],[3,1,4,1],[4,4,3,3],[8,3,5,6]]).
q([[4,3,6],[2,4,4,3]],[[4,3,6],[4,6,6,5]]).
q([[0,2,7],[3,3,6,6],[6,4,2]],[[2,4,9],[3,3,6,6],[8,6,4]]).
q([[2,5,1],[0,4,1,3]],[[2,5,1],[2,6,3,5]]).
