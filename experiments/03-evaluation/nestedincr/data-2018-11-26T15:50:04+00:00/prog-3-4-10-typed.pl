:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_flatten2(A,B):-flatten(A,B).
my_set3(A):-list_to_set(A,A).
my_odd4(A):-1 is A mod 2.
my_even5(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_set3,[list(_)]).
prim(my_odd4,[int]).
prim(my_even5,[int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(int)),list(list(int))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([[3,6,2,2],[2,7,2,2],[2,5,5,5],[5,3,2,6]],[[5,8,4,4],[4,9,4,4],[4,7,7,7],[7,5,4,8]]).
p([[4,6,6],[7,6,7]],[[6,8,8],[9,8,9]]).
p([[5,2,3,0],[2,5,0],[1,0,2],[5,5,4]],[[7,4,5,2],[4,7,2],[3,2,4],[7,7,6]]).
p([[5,4,1,0],[1,3,7],[5,6,6,7]],[[7,6,3,2],[3,5,9],[7,8,8,9]]).
p([[4,7,6,1],[7,4,7]],[[6,9,8,3],[9,6,9]]).
q([[3,3,3],[0,4,2,2],[5,7,2,6]],[[3,3,3],[2,6,4,4],[7,9,4,8]]).
q([[3,1,1,5],[6,2,7]],[[5,3,3,7],[6,2,7]]).
q([[4,1,6],[6,4,7],[1,7,0],[7,6,2,0]],[[4,1,6],[8,6,9],[3,9,2],[9,8,4,2]]).
q([[7,2,7],[2,4,6,6]],[[9,4,9],[2,4,6,6]]).
q([[6,4,3,1],[1,0,3]],[[8,6,5,3],[1,0,3]]).
