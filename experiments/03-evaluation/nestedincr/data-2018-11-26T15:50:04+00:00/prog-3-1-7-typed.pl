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
my_head2([H|_],H).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
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
p([[0,1,1,2],[5,1,1,0],[3,0,6],[6,6,1,5]],[[2,3,3,4],[7,3,3,2],[5,2,8],[8,8,3,7]]).
p([[5,7,5,5],[3,2,6,0]],[[7,9,7,7],[5,4,8,2]]).
p([[6,5,6,3],[0,6,0]],[[8,7,8,5],[2,8,2]]).
p([[4,1,3,4],[4,4,3,2],[4,1,2,0]],[[6,3,5,6],[6,6,5,4],[6,3,4,2]]).
p([[0,4,2],[6,7,5,4]],[[2,6,4],[8,9,7,6]]).
q([[1,0,5],[6,5,4],[3,3,1,6]],[[1,0,5],[8,7,6],[5,5,3,8]]).
q([[6,0,7,4],[2,6,7,3]],[[8,2,9,6],[2,6,7,3]]).
q([[0,1,7],[4,0,0,4],[5,4,4,6]],[[0,1,7],[6,2,2,6],[7,6,6,8]]).
q([[1,2,7,4],[2,7,4]],[[3,4,9,6],[2,7,4]]).
q([[4,3,4,3],[2,6,7]],[[6,5,6,5],[2,6,7]]).
