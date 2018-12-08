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
my_max_list3(A,B):-max_list(A,B).
my_odd4(A):-1 is A mod 2.
my_element5(A,B):-member(B,A).
my_msort6(A,B):-msort(A,B).
my_set7(A):-list_to_set(A,A).
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_odd4,[int]).
prim(my_element5,[list(T),T]).
prim(my_msort6,[list(int),list(int)]).
prim(my_set7,[list(_)]).
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
p([[7,7,0,0],[6,3,2,6]],[[9,9,2,2],[8,5,4,8]]).
p([[3,0,1,2],[4,6,4]],[[5,2,3,4],[6,8,6]]).
p([[1,1,0],[3,7,3],[0,4,7]],[[3,3,2],[5,9,5],[2,6,9]]).
p([[5,2,7,1],[7,0,5],[0,4,6],[2,5,3]],[[7,4,9,3],[9,2,7],[2,6,8],[4,7,5]]).
p([[7,3,3,1],[4,5,6,3],[3,6,4,5]],[[9,5,5,3],[6,7,8,5],[5,8,6,7]]).
q([[3,1,1,3],[1,6,5,2]],[[5,3,3,5],[1,6,5,2]]).
q([[5,5,0],[3,5,7,0],[5,7,6,2]],[[7,7,2],[3,5,7,0],[7,9,8,4]]).
q([[6,3,6],[3,1,5,3],[1,6,2],[2,5,0,1]],[[6,3,6],[5,3,7,5],[1,6,2],[4,7,2,3]]).
q([[4,5,3,4],[4,3,2],[5,4,0]],[[6,7,5,6],[6,5,4],[5,4,0]]).
q([[7,0,3],[1,0,4]],[[7,0,3],[3,2,6]]).
