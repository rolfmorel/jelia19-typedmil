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
my_last2(A,B):-last(A,B).
my_len3(A,B):-length(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_double4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
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
p([[5,0,0,7],[7,3,4,1],[5,4,5,3]],[[7,2,2,9],[9,5,6,3],[7,6,7,5]]).
p([[1,1,0,6],[1,0,6],[4,4,0]],[[3,3,2,8],[3,2,8],[6,6,2]]).
p([[6,2,4],[5,0,5],[7,7,7]],[[8,4,6],[7,2,7],[9,9,9]]).
p([[3,5,0,4],[6,2,7],[1,5,0],[6,3,3,4]],[[5,7,2,6],[8,4,9],[3,7,2],[8,5,5,6]]).
p([[2,5,1,0],[3,2,6,2],[6,5,1],[5,7,7,1]],[[4,7,3,2],[5,4,8,4],[8,7,3],[7,9,9,3]]).
q([[7,3,6],[7,4,7],[2,2,6],[7,2,7,2]],[[9,5,8],[7,4,7],[4,4,8],[7,2,7,2]]).
q([[1,5,2],[1,0,1],[5,5,0],[4,5,6,1]],[[1,5,2],[3,2,3],[5,5,0],[6,7,8,3]]).
q([[3,2,1,2],[0,2,4],[1,0,4,4]],[[5,4,3,4],[0,2,4],[3,2,6,6]]).
q([[6,2,7],[6,5,6]],[[8,4,9],[6,5,6]]).
q([[2,4,0],[3,7,1],[1,0,3,1]],[[2,4,0],[5,9,3],[3,2,5,3]]).
