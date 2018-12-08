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
my_set2(A):-list_to_set(A,A).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_set2,[list(_)]).
prim(my_toupper3,[char,char]).
prim(my_uppercase4,[char]).
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
p([[7,2,2,7],[3,2,0],[6,0,5,3],[5,1,7]],[[9,4,4,9],[5,4,2],[8,2,7,5],[7,3,9]]).
p([[0,6,3],[1,3,1],[7,4,4,0],[6,0,1,4]],[[2,8,5],[3,5,3],[9,6,6,2],[8,2,3,6]]).
p([[7,0,7,5],[4,4,6,2],[6,7,5,4],[1,7,6,3]],[[9,2,9,7],[6,6,8,4],[8,9,7,6],[3,9,8,5]]).
p([[0,0,6],[2,6,6],[4,4,3,4],[0,1,2,4]],[[2,2,8],[4,8,8],[6,6,5,6],[2,3,4,6]]).
p([[6,1,4,2],[1,6,7]],[[8,3,6,4],[3,8,9]]).
q([[1,0,3,3],[0,7,3]],[[1,0,3,3],[2,9,5]]).
q([[5,6,7],[2,7,4,4],[6,1,1]],[[5,6,7],[4,9,6,6],[8,3,3]]).
q([[2,0,2],[3,4,5]],[[4,2,4],[3,4,5]]).
q([[3,0,4],[2,5,4],[5,4,7,3],[5,5,2,4]],[[5,2,6],[2,5,4],[7,6,9,5],[7,7,4,6]]).
q([[1,1,5,7],[5,4,6,1],[0,6,7],[2,0,4]],[[3,3,7,9],[7,6,8,3],[0,6,7],[2,0,4]]).
