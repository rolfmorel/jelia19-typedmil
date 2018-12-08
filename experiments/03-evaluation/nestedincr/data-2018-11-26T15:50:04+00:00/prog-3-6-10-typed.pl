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
my_sumlist2(A,B):-sumlist(A,B).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_msort4(A,B):-msort(A,B).
my_set5(A):-list_to_set(A,A).
my_element6(A,B):-member(B,A).
my_even7(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_lowercase3,[char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_set5,[list(_)]).
prim(my_element6,[list(T),T]).
prim(my_even7,[int]).
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
p([[1,0,4],[3,4,6,7],[2,3,4]],[[3,2,6],[5,6,8,9],[4,5,6]]).
p([[7,5,2,0],[6,6,1,2],[2,6,7,3],[5,0,1]],[[9,7,4,2],[8,8,3,4],[4,8,9,5],[7,2,3]]).
p([[1,4,2],[2,1,7],[0,6,5,6]],[[3,6,4],[4,3,9],[2,8,7,8]]).
p([[6,6,3],[5,0,3],[7,4,4],[1,6,7]],[[8,8,5],[7,2,5],[9,6,6],[3,8,9]]).
p([[3,3,4],[7,1,3],[2,1,5,3]],[[5,5,6],[9,3,5],[4,3,7,5]]).
q([[7,2,5],[2,7,2],[7,0,2,4]],[[9,4,7],[4,9,4],[7,0,2,4]]).
q([[2,7,7],[4,2,0],[0,4,4],[0,2,6,2]],[[2,7,7],[6,4,2],[2,6,6],[0,2,6,2]]).
q([[2,1,4],[0,0,7,6],[0,2,3,2],[7,4,0]],[[4,3,6],[2,2,9,8],[0,2,3,2],[9,6,2]]).
q([[3,6,1,5],[5,3,7,0],[1,1,4,5]],[[5,8,3,7],[5,3,7,0],[3,3,6,7]]).
q([[0,0,3],[2,4,7],[7,6,0]],[[2,2,5],[2,4,7],[9,8,2]]).
