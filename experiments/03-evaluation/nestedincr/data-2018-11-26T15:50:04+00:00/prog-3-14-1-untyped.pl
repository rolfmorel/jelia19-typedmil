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
my_toupper2(A,B):-upcase_atom(A,B),char_code(A,_).
my_element3(A,B):-member(B,A).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist5(A,B):-sumlist(A,B).
my_tail6([_|TL],TL).
my_double7(N,M):-M is 2*N,M =< 10.
my_msort8(A,B):-msort(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_even11(A):-0 is A mod 2.
my_max_list12(A,B):-max_list(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_flatten14(A,B):-flatten(A,B).
my_set15(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_toupper2/2).
prim(my_element3/2).
prim(my_lowercase4/1).
prim(my_sumlist5/2).
prim(my_tail6/2).
prim(my_double7/2).
prim(my_msort8/2).
prim(my_reverse9/2).
prim(my_pred10/2).
prim(my_even11/1).
prim(my_max_list12/2).
prim(my_list_to_set13/2).
prim(my_flatten14/2).
prim(my_set15/1).
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
p([[4,2,7,5],[0,7,1,1]],[[6,4,9,7],[2,9,3,3]]).
p([[3,7,7,3],[5,2,0,0],[7,7,0,2]],[[5,9,9,5],[7,4,2,2],[9,9,2,4]]).
p([[3,7,6],[5,4,3],[0,2,0,7]],[[5,9,8],[7,6,5],[2,4,2,9]]).
p([[3,2,0],[2,2,3],[6,7,7],[1,5,2]],[[5,4,2],[4,4,5],[8,9,9],[3,7,4]]).
p([[4,3,4],[5,5,4,7],[6,2,3]],[[6,5,6],[7,7,6,9],[8,4,5]]).
q([[3,6,7],[3,3,7,1],[0,4,2]],[[5,8,9],[3,3,7,1],[2,6,4]]).
q([[6,4,7],[6,0,3,7],[5,5,1],[6,7,6,3]],[[8,6,9],[6,0,3,7],[5,5,1],[8,9,8,5]]).
q([[0,1,3,5],[7,4,6,5]],[[0,1,3,5],[9,6,8,7]]).
q([[5,6,1],[1,4,2],[0,1,2,4]],[[7,8,3],[3,6,4],[0,1,2,4]]).
q([[2,5,3],[4,6,7],[6,3,7],[4,7,1]],[[4,7,5],[4,6,7],[8,5,9],[6,9,3]]).
