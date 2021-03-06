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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_flatten3(A,B):-flatten(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_toupper5(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist6(A,B):-sumlist(A,B).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_toupper5,[char,char]).
prim(my_sumlist6,[list(int),int]).
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
p([[0,6,6,5],[7,3,5,7]],[[2,8,8,7],[9,5,7,9]]).
p([[5,5,3,0],[0,4,5,6],[0,7,0]],[[7,7,5,2],[2,6,7,8],[2,9,2]]).
p([[5,2,7,2],[5,7,0,0],[0,5,1,6]],[[7,4,9,4],[7,9,2,2],[2,7,3,8]]).
p([[7,3,6],[5,5,7,4],[6,6,3]],[[9,5,8],[7,7,9,6],[8,8,5]]).
p([[4,0,5],[6,4,0],[2,5,1,1],[3,1,7,1]],[[6,2,7],[8,6,2],[4,7,3,3],[5,3,9,3]]).
q([[3,4,1],[6,6,0]],[[3,4,1],[8,8,2]]).
q([[6,6,2,0],[7,6,1]],[[8,8,4,2],[7,6,1]]).
q([[7,3,0,3],[6,2,0,1]],[[7,3,0,3],[8,4,2,3]]).
q([[2,0,7,0],[3,3,5],[0,0,3]],[[4,2,9,2],[5,5,7],[0,0,3]]).
q([[4,5,6,2],[1,4,1],[6,3,4]],[[6,7,8,4],[1,4,1],[8,5,6]]).
