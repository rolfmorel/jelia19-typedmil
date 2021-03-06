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
my_pred3(A,B):-succ(B,A),A > 0.
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_odd5(A):-1 is A mod 2.
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_pred3,[int,int]).
prim(my_toupper4,[char,char]).
prim(my_odd5,[int]).
prim(my_tolower6,[char,char]).
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
p([[2,0,3,2],[2,7,0]],[[4,2,5,4],[4,9,2]]).
p([[3,5,3],[7,5,1,2],[4,1,0,2]],[[5,7,5],[9,7,3,4],[6,3,2,4]]).
p([[1,4,2],[5,5,2,5],[1,5,6,1],[5,3,0,4]],[[3,6,4],[7,7,4,7],[3,7,8,3],[7,5,2,6]]).
p([[0,0,5,2],[2,5,6,7],[1,5,2,3]],[[2,2,7,4],[4,7,8,9],[3,7,4,5]]).
p([[5,6,1,2],[1,2,1,2]],[[7,8,3,4],[3,4,3,4]]).
q([[2,1,6],[1,0,7,3],[4,4,4]],[[4,3,8],[3,2,9,5],[4,4,4]]).
q([[0,4,3],[6,5,4,0]],[[0,4,3],[8,7,6,2]]).
q([[5,7,3,5],[4,7,2],[0,7,7],[0,7,4]],[[7,9,5,7],[4,7,2],[2,9,9],[2,9,6]]).
q([[6,6,5],[2,6,4],[3,7,2,0],[3,7,2]],[[6,6,5],[2,6,4],[5,9,4,2],[5,9,4]]).
q([[6,6,3,0],[4,1,7],[5,4,3,0]],[[8,8,5,2],[4,1,7],[7,6,5,2]]).
