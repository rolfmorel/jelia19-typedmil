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
my_msort3(A,B):-msort(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_list_to_set4,[list(T),list(T)]).
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
p([[5,1,2,7],[0,0,6]],[[7,3,4,9],[2,2,8]]).
p([[3,3,4,7],[4,4,6],[3,7,7]],[[5,5,6,9],[6,6,8],[5,9,9]]).
p([[2,4,0],[7,5,1]],[[4,6,2],[9,7,3]]).
p([[3,6,5,0],[1,1,3,2],[7,1,2,3],[1,3,6]],[[5,8,7,2],[3,3,5,4],[9,3,4,5],[3,5,8]]).
p([[3,4,2,2],[4,2,4],[4,0,4,3],[1,2,2]],[[5,6,4,4],[6,4,6],[6,2,6,5],[3,4,4]]).
q([[4,0,3,2],[1,6,0],[0,6,4,0],[0,0,5]],[[6,2,5,4],[3,8,2],[2,8,6,2],[0,0,5]]).
q([[7,0,6],[2,2,3]],[[9,2,8],[2,2,3]]).
q([[5,2,5],[2,7,0,0],[4,1,2]],[[5,2,5],[4,9,2,2],[6,3,4]]).
q([[3,3,2,5],[6,0,2,4],[1,3,0,0],[1,2,0,4]],[[5,5,4,7],[8,2,4,6],[1,3,0,0],[3,4,2,6]]).
q([[1,0,0],[3,7,6]],[[3,2,2],[3,7,6]]).
