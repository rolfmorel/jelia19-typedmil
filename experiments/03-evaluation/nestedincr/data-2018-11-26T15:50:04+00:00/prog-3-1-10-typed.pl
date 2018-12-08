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
my_set2(A):-list_to_set(A,A).
prim(my_succ1,[int,int]).
prim(my_set2,[list(_)]).
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
p([[5,2,2],[0,0,2,1]],[[7,4,4],[2,2,4,3]]).
p([[0,1,3],[5,4,1,5],[5,6,3,0],[6,2,2,7]],[[2,3,5],[7,6,3,7],[7,8,5,2],[8,4,4,9]]).
p([[0,2,0,0],[4,5,7],[6,5,6,5]],[[2,4,2,2],[6,7,9],[8,7,8,7]]).
p([[0,1,0],[3,0,0,3],[6,2,0,2],[4,4,4]],[[2,3,2],[5,2,2,5],[8,4,2,4],[6,6,6]]).
p([[0,5,3,5],[5,2,5]],[[2,7,5,7],[7,4,7]]).
q([[3,2,5],[2,0,0,0],[6,7,1,3],[5,1,6]],[[5,4,7],[2,0,0,0],[8,9,3,5],[5,1,6]]).
q([[3,6,4,7],[3,3,5]],[[3,6,4,7],[5,5,7]]).
q([[5,2,7,6],[0,3,1]],[[7,4,9,8],[0,3,1]]).
q([[4,4,1],[2,7,6],[2,6,5,5]],[[6,6,3],[2,7,6],[4,8,7,7]]).
q([[1,3,3,0],[3,5,7,7],[5,2,0],[1,6,0]],[[3,5,5,2],[5,7,9,9],[7,4,2],[1,6,0]]).
