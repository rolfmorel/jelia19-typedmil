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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_last3(A,B):-last(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_min_list5(A,B):-min_list(A,B).
my_flatten6(A,B):-flatten(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_element8(A,B):-member(B,A).
my_tail9([_|TL],TL).
prim(my_succ1,[int,int]).
prim(my_lowercase2,[char]).
prim(my_last3,[list(T),T]).
prim(my_double4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_element8,[list(T),T]).
prim(my_tail9,[list(T),list(T)]).
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
p([[7,2,2],[6,1,7],[1,6,1,2],[6,7,5]],[[9,4,4],[8,3,9],[3,8,3,4],[8,9,7]]).
p([[0,0,7,6],[2,1,2]],[[2,2,9,8],[4,3,4]]).
p([[0,0,3],[7,1,2],[0,0,4],[3,3,2]],[[2,2,5],[9,3,4],[2,2,6],[5,5,4]]).
p([[3,1,1],[7,7,2],[2,0,5,2]],[[5,3,3],[9,9,4],[4,2,7,4]]).
p([[4,1,5],[2,2,2,2],[6,1,1]],[[6,3,7],[4,4,4,4],[8,3,3]]).
q([[2,6,5],[4,0,0]],[[2,6,5],[6,2,2]]).
q([[7,4,4],[4,5,3],[4,2,6]],[[9,6,6],[4,5,3],[6,4,8]]).
q([[0,0,0,1],[7,6,0]],[[2,2,2,3],[7,6,0]]).
q([[0,4,7,2],[3,7,7,0],[0,7,4,1]],[[0,4,7,2],[5,9,9,2],[2,9,6,3]]).
q([[7,1,5],[2,0,5]],[[9,3,7],[2,0,5]]).
