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
my_element3(A,B):-member(B,A).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_toupper5(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_element3,[list(T),T]).
prim(my_lowercase4,[char]).
prim(my_toupper5,[char,char]).
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
p([[1,5,7,6],[3,4,4],[5,2,4,7],[7,0,6]],[[3,7,9,8],[5,6,6],[7,4,6,9],[9,2,8]]).
p([[7,5,6,2],[1,7,7,0],[5,6,2,0]],[[9,7,8,4],[3,9,9,2],[7,8,4,2]]).
p([[7,7,6,6],[5,4,6,2],[4,6,4]],[[9,9,8,8],[7,6,8,4],[6,8,6]]).
p([[2,5,7,1],[6,5,4,3],[5,4,5,0]],[[4,7,9,3],[8,7,6,5],[7,6,7,2]]).
p([[7,4,0],[7,4,2,4],[7,0,2,1],[4,0,7,6]],[[9,6,2],[9,6,4,6],[9,2,4,3],[6,2,9,8]]).
q([[7,5,0,5],[1,2,4]],[[9,7,2,7],[1,2,4]]).
q([[2,7,6],[2,6,7],[0,2,3,2]],[[4,9,8],[4,8,9],[0,2,3,2]]).
q([[5,4,4,5],[4,4,2,1],[2,1,2,4],[3,7,4]],[[7,6,6,7],[6,6,4,3],[2,1,2,4],[3,7,4]]).
q([[2,1,3],[5,2,5,6]],[[4,3,5],[5,2,5,6]]).
q([[5,5,0],[0,7,7],[5,2,1]],[[7,7,2],[0,7,7],[7,4,3]]).
