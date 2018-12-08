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
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_even7(A):-0 is A mod 2.
my_min_list8(A,B):-min_list(A,B).
my_msort9(A,B):-msort(A,B).
my_element10(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_lowercase4/1).
prim(my_list_to_set5/2).
prim(my_uppercase6/1).
prim(my_even7/1).
prim(my_min_list8/2).
prim(my_msort9/2).
prim(my_element10/2).
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
p([[1,4,5],[4,7,3],[1,6,6],[4,5,7]],[[3,6,7],[6,9,5],[3,8,8],[6,7,9]]).
p([[5,1,5,2],[5,6,2,2],[7,3,5,2],[7,3,4,3]],[[7,3,7,4],[7,8,4,4],[9,5,7,4],[9,5,6,5]]).
p([[3,0,2],[5,1,4,6]],[[5,2,4],[7,3,6,8]]).
p([[0,1,7,2],[5,7,6,4],[5,3,6,2],[1,2,1]],[[2,3,9,4],[7,9,8,6],[7,5,8,4],[3,4,3]]).
p([[3,5,1],[7,3,4],[3,2,2,3],[7,5,7,5]],[[5,7,3],[9,5,6],[5,4,4,5],[9,7,9,7]]).
q([[7,7,2],[7,4,6,5],[5,1,6,5]],[[7,7,2],[9,6,8,7],[7,3,8,7]]).
q([[4,4,7],[7,2,3,2],[1,1,6],[0,2,0]],[[6,6,9],[9,4,5,4],[3,3,8],[0,2,0]]).
q([[3,5,2],[1,1,2,3]],[[5,7,4],[1,1,2,3]]).
q([[3,5,0,3],[6,5,3],[0,2,2,7]],[[5,7,2,5],[8,7,5],[0,2,2,7]]).
q([[4,7,0,0],[3,6,6],[1,7,1],[4,3,2]],[[6,9,2,2],[3,6,6],[3,9,3],[4,3,2]]).
