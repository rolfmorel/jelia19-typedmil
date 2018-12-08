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
my_tail2([_|TL],TL).
my_odd3(A):-1 is A mod 2.
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist5(A,B):-sumlist(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_double7(N,M):-M is 2*N,M =< 10.
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_even10(A):-0 is A mod 2.
my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_odd3/1).
prim(my_tolower4/2).
prim(my_sumlist5/2).
prim(my_uppercase6/1).
prim(my_double7/2).
prim(my_reverse8/2).
prim(my_last9/2).
prim(my_even10/1).
prim(my_lowercase11/1).
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
p([[3,4,6,4],[7,7,1,7],[7,0,5,4]],[[5,6,8,6],[9,9,3,9],[9,2,7,6]]).
p([[1,6,5],[4,3,5]],[[3,8,7],[6,5,7]]).
p([[1,0,0],[3,7,6],[7,4,5]],[[3,2,2],[5,9,8],[9,6,7]]).
p([[1,1,3],[0,4,2],[3,4,3],[4,5,3]],[[3,3,5],[2,6,4],[5,6,5],[6,7,5]]).
p([[7,2,1,1],[4,1,0],[5,4,1,0],[4,5,1,5]],[[9,4,3,3],[6,3,2],[7,6,3,2],[6,7,3,7]]).
q([[4,0,0],[2,0,0],[7,3,5,5]],[[4,0,0],[4,2,2],[9,5,7,7]]).
q([[6,3,1,0],[6,7,7,7],[0,4,4]],[[6,3,1,0],[8,9,9,9],[2,6,6]]).
q([[5,4,6],[7,1,3,1]],[[5,4,6],[9,3,5,3]]).
q([[3,2,0,2],[6,5,7],[2,3,5,7]],[[3,2,0,2],[8,7,9],[4,5,7,9]]).
q([[5,3,5],[2,1,5],[2,1,7]],[[7,5,7],[2,1,5],[4,3,9]]).
