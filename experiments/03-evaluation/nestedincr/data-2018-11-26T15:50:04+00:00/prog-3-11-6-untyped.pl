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
my_pred2(A,B):-succ(B,A),A > 0.
my_sumlist3(A,B):-sumlist(A,B).
my_tail4([_|TL],TL).
my_msort5(A,B):-msort(A,B).
my_head6([H|_],H).
my_len7(A,B):-length(A,B).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_double9(N,M):-M is 2*N,M =< 10.
my_last10(A,B):-last(A,B).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten12(A,B):-flatten(A,B).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
prim(my_tail4/2).
prim(my_msort5/2).
prim(my_head6/2).
prim(my_len7/2).
prim(my_uppercase8/1).
prim(my_double9/2).
prim(my_last10/2).
prim(my_tolower11/2).
prim(my_flatten12/2).
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
p([[5,7,3,7],[6,7,2,5]],[[7,9,5,9],[8,9,4,7]]).
p([[1,3,0],[6,5,0,0],[1,3,3],[2,5,3]],[[3,5,2],[8,7,2,2],[3,5,5],[4,7,5]]).
p([[2,4,0,1],[2,7,7],[4,4,6]],[[4,6,2,3],[4,9,9],[6,6,8]]).
p([[5,5,2,2],[6,0,3]],[[7,7,4,4],[8,2,5]]).
p([[1,4,2],[6,3,4],[4,4,5,1],[4,1,6,0]],[[3,6,4],[8,5,6],[6,6,7,3],[6,3,8,2]]).
q([[6,6,3,2],[5,7,1],[2,1,7],[1,7,6,3]],[[6,6,3,2],[7,9,3],[4,3,9],[3,9,8,5]]).
q([[5,4,3,2],[1,3,1]],[[5,4,3,2],[3,5,3]]).
q([[6,3,7,6],[3,4,1],[4,3,2],[4,6,4,3]],[[8,5,9,8],[5,6,3],[4,3,2],[4,6,4,3]]).
q([[5,7,2,7],[6,5,6,6],[4,1,5],[6,2,4,1]],[[7,9,4,9],[6,5,6,6],[4,1,5],[8,4,6,3]]).
q([[7,6,1,0],[3,0,2],[7,2,0,5],[0,0,0,7]],[[7,6,1,0],[5,2,4],[9,4,2,7],[0,0,0,7]]).
