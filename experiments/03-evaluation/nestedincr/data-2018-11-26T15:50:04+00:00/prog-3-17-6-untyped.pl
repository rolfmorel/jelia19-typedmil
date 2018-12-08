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
my_len2(A,B):-length(A,B).
my_set3(A):-list_to_set(A,A).
my_even4(A):-0 is A mod 2.
my_head5([H|_],H).
my_last6(A,B):-last(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_tail8([_|TL],TL).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_min_list10(A,B):-min_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_msort13(A,B):-msort(A,B).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten16(A,B):-flatten(A,B).
my_reverse17(A,B):-reverse(A,B).
my_odd18(A):-1 is A mod 2.
prim(my_succ1/2).
prim(my_len2/2).
prim(my_set3/1).
prim(my_even4/1).
prim(my_head5/2).
prim(my_last6/2).
prim(my_list_to_set7/2).
prim(my_tail8/2).
prim(my_lowercase9/1).
prim(my_min_list10/2).
prim(my_max_list11/2).
prim(my_sumlist12/2).
prim(my_msort13/2).
prim(my_toupper14/2).
prim(my_tolower15/2).
prim(my_flatten16/2).
prim(my_reverse17/2).
prim(my_odd18/1).
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
p([[2,2,0,6],[7,4,2]],[[4,4,2,8],[9,6,4]]).
p([[4,3,3,6],[1,6,2],[2,5,4],[5,3,7]],[[6,5,5,8],[3,8,4],[4,7,6],[7,5,9]]).
p([[4,1,5,0],[3,2,6],[5,4,4,2]],[[6,3,7,2],[5,4,8],[7,6,6,4]]).
p([[3,3,7,3],[3,0,2,5],[4,7,1,7]],[[5,5,9,5],[5,2,4,7],[6,9,3,9]]).
p([[6,3,2],[3,1,2]],[[8,5,4],[5,3,4]]).
q([[1,5,3],[3,5,5]],[[3,7,5],[3,5,5]]).
q([[4,1,7],[2,4,7,4],[1,2,0,4],[4,5,6,4]],[[6,3,9],[2,4,7,4],[3,4,2,6],[6,7,8,6]]).
q([[7,1,5],[3,0,7]],[[7,1,5],[5,2,9]]).
q([[6,0,0],[3,3,2],[4,4,1],[5,3,3,1]],[[6,0,0],[5,5,4],[6,6,3],[5,3,3,1]]).
q([[3,5,0],[4,2,1],[3,7,7,0]],[[3,5,0],[6,4,3],[5,9,9,2]]).
