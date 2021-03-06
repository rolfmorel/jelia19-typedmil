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
my_msort2(A,B):-msort(A,B).
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_tolower3,[char,char]).
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
p([[1,1,2],[2,3,4,4],[1,1,3,7],[0,3,4]],[[3,3,4],[4,5,6,6],[3,3,5,9],[2,5,6]]).
p([[2,1,2,1],[3,0,3,0]],[[4,3,4,3],[5,2,5,2]]).
p([[0,4,6],[1,4,6],[5,1,1,0]],[[2,6,8],[3,6,8],[7,3,3,2]]).
p([[3,1,4],[7,1,4,2],[5,5,3],[0,1,2]],[[5,3,6],[9,3,6,4],[7,7,5],[2,3,4]]).
p([[7,2,1],[1,5,4],[3,1,7,4]],[[9,4,3],[3,7,6],[5,3,9,6]]).
q([[3,3,6,2],[4,6,1,1]],[[5,5,8,4],[4,6,1,1]]).
q([[0,2,3,2],[0,5,5,5],[4,4,2,1]],[[0,2,3,2],[2,7,7,7],[6,6,4,3]]).
q([[4,7,0,1],[1,4,1]],[[6,9,2,3],[1,4,1]]).
q([[3,3,3],[5,7,0,4],[2,1,0,3]],[[3,3,3],[7,9,2,6],[4,3,2,5]]).
q([[0,4,7,4],[7,4,4,2],[3,1,0,7]],[[2,6,9,6],[7,4,4,2],[5,3,2,9]]).
