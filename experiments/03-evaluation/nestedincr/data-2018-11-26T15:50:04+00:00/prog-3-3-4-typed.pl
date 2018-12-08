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
my_double2(N,M):-M is 2*N,M =< 10.
my_tail3([_|TL],TL).
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_tail3,[list(T),list(T)]).
prim(my_tolower4,[char,char]).
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
p([[5,2,7,5],[1,6,4,0]],[[7,4,9,7],[3,8,6,2]]).
p([[0,0,2],[4,7,2,0],[5,7,5],[0,7,2]],[[2,2,4],[6,9,4,2],[7,9,7],[2,9,4]]).
p([[3,1,2],[6,6,3]],[[5,3,4],[8,8,5]]).
p([[3,7,1,3],[4,4,2],[0,4,5,2]],[[5,9,3,5],[6,6,4],[2,6,7,4]]).
p([[6,0,2],[1,6,4,1]],[[8,2,4],[3,8,6,3]]).
q([[5,6,1],[6,4,6],[1,7,2,6]],[[7,8,3],[8,6,8],[1,7,2,6]]).
q([[1,6,0,7],[7,7,3,2]],[[1,6,0,7],[9,9,5,4]]).
q([[4,1,5],[7,4,5],[4,2,5]],[[4,1,5],[9,6,7],[6,4,7]]).
q([[1,7,0,5],[4,1,1],[1,5,0,3]],[[1,7,0,5],[6,3,3],[3,7,2,5]]).
q([[2,4,7],[4,6,2],[7,3,4]],[[2,4,7],[6,8,4],[9,5,6]]).
