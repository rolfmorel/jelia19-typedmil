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
my_pred3(A,B):-succ(B,A),A > 0.
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_odd2,[int]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
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
p([[1,4,3],[7,3,6],[3,5,6,1]],[[3,6,5],[9,5,8],[5,7,8,3]]).
p([[5,1,7],[0,5,3,1],[3,2,0],[0,3,2]],[[7,3,9],[2,7,5,3],[5,4,2],[2,5,4]]).
p([[1,0,6],[1,0,6],[5,5,5,0]],[[3,2,8],[3,2,8],[7,7,7,2]]).
p([[7,3,3],[5,6,4,2],[5,7,4]],[[9,5,5],[7,8,6,4],[7,9,6]]).
p([[6,2,7,6],[6,6,0,1],[6,6,5],[3,5,4]],[[8,4,9,8],[8,8,2,3],[8,8,7],[5,7,6]]).
q([[5,7,2,1],[4,7,6,0],[6,6,0],[3,7,4]],[[5,7,2,1],[6,9,8,2],[8,8,2],[5,9,6]]).
q([[5,7,4],[2,2,6],[5,4,6,4],[0,1,4]],[[5,7,4],[4,4,8],[7,6,8,6],[2,3,6]]).
q([[4,5,5],[1,4,4,5],[6,5,6]],[[4,5,5],[3,6,6,7],[8,7,8]]).
q([[3,3,6],[6,0,2],[4,2,7]],[[3,3,6],[8,2,4],[6,4,9]]).
q([[1,7,1],[4,6,6],[0,7,5,7],[4,5,4]],[[3,9,3],[4,6,6],[2,9,7,9],[6,7,6]]).
