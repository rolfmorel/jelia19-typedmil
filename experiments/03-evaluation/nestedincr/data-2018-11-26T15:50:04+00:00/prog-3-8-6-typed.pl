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
my_max_list2(A,B):-max_list(A,B).
my_odd3(A):-1 is A mod 2.
my_flatten4(A,B):-flatten(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_set6(A):-list_to_set(A,A).
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set8(A,B):-list_to_set(A,B).
my_head9([H|_],H).
prim(my_succ1,[int,int]).
prim(my_max_list2,[list(int),int]).
prim(my_odd3,[int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_double5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_uppercase7,[char]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_head9,[list(T),T]).
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
p([[4,7,6,0],[1,6,3]],[[6,9,8,2],[3,8,5]]).
p([[6,2,3,0],[3,1,6],[7,4,4]],[[8,4,5,2],[5,3,8],[9,6,6]]).
p([[1,3,2,1],[7,4,2]],[[3,5,4,3],[9,6,4]]).
p([[3,5,0],[5,2,3,4],[2,7,6],[1,7,5]],[[5,7,2],[7,4,5,6],[4,9,8],[3,9,7]]).
p([[5,7,2,7],[6,7,1]],[[7,9,4,9],[8,9,3]]).
q([[7,0,2,2],[2,3,6,1],[6,4,7],[3,0,3]],[[7,0,2,2],[2,3,6,1],[8,6,9],[5,2,5]]).
q([[2,1,2],[7,1,2,3]],[[4,3,4],[7,1,2,3]]).
q([[5,1,3,4],[1,1,0,5],[2,2,5]],[[5,1,3,4],[3,3,2,7],[4,4,7]]).
q([[3,2,3],[0,6,1,5],[6,6,3,6]],[[5,4,5],[2,8,3,7],[6,6,3,6]]).
q([[1,6,7],[6,4,0]],[[1,6,7],[8,6,2]]).
