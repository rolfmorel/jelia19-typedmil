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
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_odd5(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_toupper3,[char,char]).
prim(my_double4,[int,int]).
prim(my_odd5,[int]).
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
p([[0,7,0,3],[3,1,5],[6,6,3],[3,4,6]],[[2,9,2,5],[5,3,7],[8,8,5],[5,6,8]]).
p([[5,0,6],[4,1,7,5],[7,7,6],[3,0,0]],[[7,2,8],[6,3,9,7],[9,9,8],[5,2,2]]).
p([[5,0,2],[6,2,4],[3,1,5,5],[3,7,4]],[[7,2,4],[8,4,6],[5,3,7,7],[5,9,6]]).
p([[5,7,6,0],[7,2,6,6]],[[7,9,8,2],[9,4,8,8]]).
p([[0,3,4],[0,3,0]],[[2,5,6],[2,5,2]]).
q([[0,2,3,4],[1,1,0]],[[2,4,5,6],[1,1,0]]).
q([[1,0,1],[6,2,7,1],[3,2,6,5]],[[3,2,3],[6,2,7,1],[5,4,8,7]]).
q([[5,3,1],[1,1,6]],[[5,3,1],[3,3,8]]).
q([[0,7,3,3],[6,0,1],[1,0,2],[2,1,4,0]],[[2,9,5,5],[8,2,3],[1,0,2],[2,1,4,0]]).
q([[0,6,7],[2,6,5],[2,2,1,4]],[[2,8,9],[4,8,7],[2,2,1,4]]).
