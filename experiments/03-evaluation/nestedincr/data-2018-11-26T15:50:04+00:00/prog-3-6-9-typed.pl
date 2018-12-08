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
my_tail3([_|TL],TL).
my_pred4(A,B):-succ(B,A),A > 0.
my_double5(N,M):-M is 2*N,M =< 10.
my_set6(A):-list_to_set(A,A).
my_even7(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_tail3,[list(T),list(T)]).
prim(my_pred4,[int,int]).
prim(my_double5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_even7,[int]).
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
p([[3,4,2],[7,7,0],[6,4,5],[6,3,6,7]],[[5,6,4],[9,9,2],[8,6,7],[8,5,8,9]]).
p([[7,1,4,4],[0,5,7,2],[4,3,0],[0,7,1,5]],[[9,3,6,6],[2,7,9,4],[6,5,2],[2,9,3,7]]).
p([[7,5,2],[6,7,0,5],[1,7,6],[7,5,3]],[[9,7,4],[8,9,2,7],[3,9,8],[9,7,5]]).
p([[4,3,1,6],[4,6,5],[4,3,5]],[[6,5,3,8],[6,8,7],[6,5,7]]).
p([[1,5,3],[7,2,0,7],[4,0,2]],[[3,7,5],[9,4,2,9],[6,2,4]]).
q([[0,5,3,6],[7,2,1,7],[2,0,0,6],[1,6,7]],[[0,5,3,6],[9,4,3,9],[2,0,0,6],[3,8,9]]).
q([[0,5,6],[0,1,4],[1,5,5]],[[0,5,6],[2,3,6],[3,7,7]]).
q([[1,0,4],[4,3,6,2]],[[1,0,4],[6,5,8,4]]).
q([[5,1,1,5],[2,4,7,5],[1,7,4]],[[5,1,1,5],[4,6,9,7],[3,9,6]]).
q([[4,1,5],[6,0,3,5],[1,3,3,7],[2,7,3,2]],[[6,3,7],[6,0,3,5],[1,3,3,7],[4,9,5,4]]).
