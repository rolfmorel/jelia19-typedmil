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
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_reverse3,[list(T),list(T)]).
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
p([[1,2,3],[3,1,5]],[[3,4,5],[5,3,7]]).
p([[4,4,3],[0,5,4],[5,4,6,0]],[[6,6,5],[2,7,6],[7,6,8,2]]).
p([[0,4,0],[1,6,5],[5,3,6,7]],[[2,6,2],[3,8,7],[7,5,8,9]]).
p([[1,7,7],[1,2,4,3]],[[3,9,9],[3,4,6,5]]).
p([[5,0,2,3],[4,0,7],[2,0,1,6],[4,1,7,5]],[[7,2,4,5],[6,2,9],[4,2,3,8],[6,3,9,7]]).
q([[1,0,2,2],[6,2,1,1]],[[3,2,4,4],[6,2,1,1]]).
q([[4,1,5,1],[2,2,5],[4,2,0],[2,1,3]],[[4,1,5,1],[2,2,5],[6,4,2],[4,3,5]]).
q([[1,2,4,7],[4,2,6,3],[7,0,0]],[[3,4,6,9],[4,2,6,3],[9,2,2]]).
q([[6,7,7,0],[2,6,7]],[[6,7,7,0],[4,8,9]]).
q([[4,7,7],[6,7,2],[2,0,7],[6,4,2]],[[6,9,9],[6,7,2],[4,2,9],[8,6,4]]).
