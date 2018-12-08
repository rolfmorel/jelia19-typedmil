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
my_msort2(A,B):-msort(A,B).
my_odd3(A):-1 is A mod 2.
my_even4(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_odd3,[int]).
prim(my_even4,[int]).
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
p([[0,0,3],[7,4,6,6],[3,5,6,6]],[[2,2,5],[9,6,8,8],[5,7,8,8]]).
p([[4,3,1],[4,5,4]],[[6,5,3],[6,7,6]]).
p([[4,3,1],[3,7,4]],[[6,5,3],[5,9,6]]).
p([[7,4,7,1],[3,7,4],[4,5,7],[2,3,2]],[[9,6,9,3],[5,9,6],[6,7,9],[4,5,4]]).
p([[2,6,5,3],[7,4,6,0]],[[4,8,7,5],[9,6,8,2]]).
q([[6,2,6,3],[0,7,4,2],[7,7,2]],[[8,4,8,5],[2,9,6,4],[7,7,2]]).
q([[2,0,4,2],[5,6,5,2]],[[4,2,6,4],[5,6,5,2]]).
q([[6,6,0,3],[5,0,5,4],[5,1,4]],[[8,8,2,5],[5,0,5,4],[7,3,6]]).
q([[5,5,0,7],[6,0,5],[1,7,2]],[[5,5,0,7],[8,2,7],[3,9,4]]).
q([[5,5,2,5],[6,0,1,4],[6,4,4]],[[7,7,4,7],[8,2,3,6],[6,4,4]]).
