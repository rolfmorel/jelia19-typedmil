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
p([[3,4,5,0],[2,7,7],[2,0,3],[5,4,7]],[[5,6,7,2],[4,9,9],[4,2,5],[7,6,9]]).
p([[5,3,7,0],[7,6,3],[4,1,2,6]],[[7,5,9,2],[9,8,5],[6,3,4,8]]).
p([[7,0,3,0],[2,0,6]],[[9,2,5,2],[4,2,8]]).
p([[7,1,7,3],[7,3,2]],[[9,3,9,5],[9,5,4]]).
p([[3,6,2],[6,1,5],[1,6,6,6],[2,5,6]],[[5,8,4],[8,3,7],[3,8,8,8],[4,7,8]]).
q([[0,5,2,4],[5,2,0],[6,1,7,3]],[[2,7,4,6],[5,2,0],[8,3,9,5]]).
q([[6,5,4],[3,0,3,2],[7,4,3],[5,2,6]],[[6,5,4],[3,0,3,2],[9,6,5],[7,4,8]]).
q([[2,7,7],[7,3,6,0]],[[2,7,7],[9,5,8,2]]).
q([[3,4,5,4],[6,4,3],[1,4,6],[5,6,5]],[[5,6,7,6],[8,6,5],[1,4,6],[5,6,5]]).
q([[5,6,5,7],[2,6,3,0],[3,0,1,1],[6,6,5]],[[5,6,5,7],[2,6,3,0],[5,2,3,3],[8,8,7]]).
