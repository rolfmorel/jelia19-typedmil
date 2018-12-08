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
my_odd2(A):-1 is A mod 2.
my_flatten3(A,B):-flatten(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_len5(A,B):-length(A,B).
prim(my_succ1,[int,int]).
prim(my_odd2,[int]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_double4,[int,int]).
prim(my_len5,[list(_),int]).
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
p([[0,4,6],[0,7,7,5]],[[2,6,8],[2,9,9,7]]).
p([[0,7,2,5],[5,1,1,4]],[[2,9,4,7],[7,3,3,6]]).
p([[3,6,7],[4,1,1]],[[5,8,9],[6,3,3]]).
p([[1,4,3,3],[0,6,6],[0,1,0]],[[3,6,5,5],[2,8,8],[2,3,2]]).
p([[6,1,1,0],[5,6,3]],[[8,3,3,2],[7,8,5]]).
q([[7,3,6],[6,0,7,2],[0,4,3],[3,1,0,6]],[[9,5,8],[8,2,9,4],[2,6,5],[3,1,0,6]]).
q([[5,3,4,7],[7,6,4],[6,1,7,4]],[[7,5,6,9],[7,6,4],[8,3,9,6]]).
q([[2,2,0,3],[2,7,1],[5,5,0,7],[1,4,1]],[[4,4,2,5],[2,7,1],[7,7,2,9],[1,4,1]]).
q([[6,7,3,3],[7,7,6,7],[5,5,4],[0,0,4]],[[8,9,5,5],[9,9,8,9],[5,5,4],[2,2,6]]).
q([[2,2,3,1],[4,1,7]],[[4,4,5,3],[4,1,7]]).
