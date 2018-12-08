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
my_tail2([_|TL],TL).
prim(my_succ1,[int,int]).
prim(my_tail2,[list(T),list(T)]).
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
p([[6,3,2,4],[7,3,7],[1,3,0,2]],[[8,5,4,6],[9,5,9],[3,5,2,4]]).
p([[7,0,5,5],[5,6,4],[2,3,0,4]],[[9,2,7,7],[7,8,6],[4,5,2,6]]).
p([[3,2,0,6],[1,6,7]],[[5,4,2,8],[3,8,9]]).
p([[1,5,0],[2,0,2],[3,4,7,0]],[[3,7,2],[4,2,4],[5,6,9,2]]).
p([[0,3,2],[5,0,5,1]],[[2,5,4],[7,2,7,3]]).
q([[6,5,3,1],[2,7,1]],[[8,7,5,3],[2,7,1]]).
q([[1,3,3],[2,1,0,6]],[[1,3,3],[4,3,2,8]]).
q([[5,7,5],[1,5,3,6]],[[7,9,7],[1,5,3,6]]).
q([[5,6,2,7],[1,0,0,0],[2,7,6],[2,0,7]],[[7,8,4,9],[1,0,0,0],[4,9,8],[4,2,9]]).
q([[3,0,1],[1,0,7,3]],[[3,0,1],[3,2,9,5]]).
