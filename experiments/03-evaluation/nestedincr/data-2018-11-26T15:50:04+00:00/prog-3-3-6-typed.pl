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
my_reverse2(A,B):-reverse(A,B).
my_set3(A):-list_to_set(A,A).
my_even4(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_reverse2,[list(T),list(T)]).
prim(my_set3,[list(_)]).
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
p([[3,0,7],[7,0,7]],[[5,2,9],[9,2,9]]).
p([[2,3,4],[5,7,7,5],[4,6,6,0]],[[4,5,6],[7,9,9,7],[6,8,8,2]]).
p([[7,7,2],[6,0,1]],[[9,9,4],[8,2,3]]).
p([[3,6,5,1],[1,7,0,4]],[[5,8,7,3],[3,9,2,6]]).
p([[4,5,4,4],[1,2,7,7]],[[6,7,6,6],[3,4,9,9]]).
q([[4,5,3],[1,7,1],[5,7,3,7]],[[6,7,5],[3,9,3],[5,7,3,7]]).
q([[7,2,0,5],[2,4,1,6],[7,6,3,1],[6,5,1,7]],[[9,4,2,7],[4,6,3,8],[7,6,3,1],[8,7,3,9]]).
q([[3,3,4,7],[6,1,2]],[[5,5,6,9],[6,1,2]]).
q([[1,1,3,1],[2,6,0,5],[4,4,7,7]],[[3,3,5,3],[2,6,0,5],[6,6,9,9]]).
q([[4,6,6,5],[4,6,0,1],[4,3,6]],[[6,8,8,7],[4,6,0,1],[6,5,8]]).
