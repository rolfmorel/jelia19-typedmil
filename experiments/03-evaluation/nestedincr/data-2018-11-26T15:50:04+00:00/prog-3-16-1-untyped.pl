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
my_list_to_set2(A,B):-list_to_set(A,B).
my_last3(A,B):-last(A,B).
my_head4([H|_],H).
my_double5(N,M):-M is 2*N,M =< 10.
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten13(A,B):-flatten(A,B).
my_set14(A):-list_to_set(A,A).
my_msort15(A,B):-msort(A,B).
my_tail16([_|TL],TL).
my_len17(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_list_to_set2/2).
prim(my_last3/2).
prim(my_head4/2).
prim(my_double5/2).
prim(my_lowercase6/1).
prim(my_max_list7/2).
prim(my_reverse8/2).
prim(my_uppercase9/1).
prim(my_pred10/2).
prim(my_odd11/1).
prim(my_tolower12/2).
prim(my_flatten13/2).
prim(my_set14/1).
prim(my_msort15/2).
prim(my_tail16/2).
prim(my_len17/2).
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
p([[2,3,5,3],[3,4,5],[6,1,4,7],[0,2,7,3]],[[4,5,7,5],[5,6,7],[8,3,6,9],[2,4,9,5]]).
p([[7,7,4],[5,0,2,5],[6,7,1,5]],[[9,9,6],[7,2,4,7],[8,9,3,7]]).
p([[2,6,7],[7,4,2]],[[4,8,9],[9,6,4]]).
p([[7,2,1],[6,2,2],[2,1,7,0]],[[9,4,3],[8,4,4],[4,3,9,2]]).
p([[1,5,1,2],[3,0,7],[5,3,6,3],[7,3,7,1]],[[3,7,3,4],[5,2,9],[7,5,8,5],[9,5,9,3]]).
q([[4,3,0,4],[0,4,2,0],[7,6,4,4]],[[6,5,2,6],[0,4,2,0],[9,8,6,6]]).
q([[3,7,4],[7,7,0,4],[2,5,7,5]],[[5,9,6],[9,9,2,6],[2,5,7,5]]).
q([[0,3,6],[0,5,4],[0,0,1,4]],[[2,5,8],[2,7,6],[0,0,1,4]]).
q([[5,3,4],[4,1,0,5],[1,2,0,5]],[[7,5,6],[4,1,0,5],[3,4,2,7]]).
q([[1,0,0],[3,1,7]],[[1,0,0],[5,3,9]]).
