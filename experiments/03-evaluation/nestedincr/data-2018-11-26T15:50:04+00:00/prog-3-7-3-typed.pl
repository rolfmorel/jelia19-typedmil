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
my_element2(A,B):-member(B,A).
my_list_to_set3(A,B):-list_to_set(A,B).
my_tail4([_|TL],TL).
my_pred5(A,B):-succ(B,A),A > 0.
my_last6(A,B):-last(A,B).
my_msort7(A,B):-msort(A,B).
my_set8(A):-list_to_set(A,A).
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_tail4,[list(T),list(T)]).
prim(my_pred5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_msort7,[list(int),list(int)]).
prim(my_set8,[list(_)]).
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
p([[5,3,4],[5,7,1,0],[5,6,0],[6,6,0]],[[7,5,6],[7,9,3,2],[7,8,2],[8,8,2]]).
p([[5,3,0],[4,2,0,3],[6,4,1],[7,6,6]],[[7,5,2],[6,4,2,5],[8,6,3],[9,8,8]]).
p([[6,1,3,6],[1,5,3,0]],[[8,3,5,8],[3,7,5,2]]).
p([[6,3,7],[3,5,6,2]],[[8,5,9],[5,7,8,4]]).
p([[7,5,7,1],[5,0,4],[2,3,7,5]],[[9,7,9,3],[7,2,6],[4,5,9,7]]).
q([[6,4,2,3],[0,1,1,0],[1,6,2,1],[3,0,3,1]],[[8,6,4,5],[0,1,1,0],[1,6,2,1],[5,2,5,3]]).
q([[0,4,7],[1,7,7],[1,2,4],[2,4,1]],[[2,6,9],[3,9,9],[1,2,4],[2,4,1]]).
q([[6,5,0,5],[5,2,2,2],[2,7,3,6],[0,2,6]],[[8,7,2,7],[5,2,2,2],[2,7,3,6],[2,4,8]]).
q([[6,0,5],[4,1,6,5]],[[8,2,7],[4,1,6,5]]).
q([[6,6,6,0],[3,2,3],[2,7,4,2],[3,6,7,2]],[[6,6,6,0],[5,4,5],[4,9,6,4],[3,6,7,2]]).
