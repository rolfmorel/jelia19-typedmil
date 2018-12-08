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
my_even2(A):-0 is A mod 2.
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_set4(A):-list_to_set(A,A).
my_last5(A,B):-last(A,B).
my_head6([H|_],H).
prim(my_succ1,[int,int]).
prim(my_even2,[int]).
prim(my_toupper3,[char,char]).
prim(my_set4,[list(_)]).
prim(my_last5,[list(T),T]).
prim(my_head6,[list(T),T]).
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
p([[0,5,0,7],[0,5,6]],[[2,7,2,9],[2,7,8]]).
p([[7,7,1,2],[7,2,6]],[[9,9,3,4],[9,4,8]]).
p([[4,1,7],[6,4,1]],[[6,3,9],[8,6,3]]).
p([[4,1,3],[6,5,4,7],[3,1,1],[3,1,7]],[[6,3,5],[8,7,6,9],[5,3,3],[5,3,9]]).
p([[5,4,0,5],[3,3,3,7],[2,4,0,2],[5,2,0,0]],[[7,6,2,7],[5,5,5,9],[4,6,2,4],[7,4,2,2]]).
q([[4,4,0],[0,1,1],[0,0,6]],[[6,6,2],[2,3,3],[0,0,6]]).
q([[7,0,2],[3,0,0]],[[9,2,4],[3,0,0]]).
q([[4,3,7],[4,6,5,7]],[[6,5,9],[4,6,5,7]]).
q([[3,7,3,4],[3,6,0],[0,6,1],[7,3,6,3]],[[3,7,3,4],[5,8,2],[2,8,3],[9,5,8,5]]).
q([[1,2,0],[1,0,6],[4,4,6],[7,5,4,5]],[[1,2,0],[3,2,8],[6,6,8],[7,5,4,5]]).
