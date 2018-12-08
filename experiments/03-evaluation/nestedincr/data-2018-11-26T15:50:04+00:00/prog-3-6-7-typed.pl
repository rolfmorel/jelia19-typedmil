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
my_sumlist3(A,B):-sumlist(A,B).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
my_msort5(A,B):-msort(A,B).
my_element6(A,B):-member(B,A).
my_head7([H|_],H).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_uppercase4,[char]).
prim(my_msort5,[list(int),list(int)]).
prim(my_element6,[list(T),T]).
prim(my_head7,[list(T),T]).
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
p([[1,1,3,3],[4,2,3,2],[4,3,2,1]],[[3,3,5,5],[6,4,5,4],[6,5,4,3]]).
p([[3,4,6,3],[5,1,6]],[[5,6,8,5],[7,3,8]]).
p([[2,7,6,7],[4,1,6,3]],[[4,9,8,9],[6,3,8,5]]).
p([[5,1,1,1],[3,4,5],[4,7,4]],[[7,3,3,3],[5,6,7],[6,9,6]]).
p([[2,0,7],[7,1,5]],[[4,2,9],[9,3,7]]).
q([[5,6,2,7],[3,2,7,1],[7,0,6,2]],[[5,6,2,7],[5,4,9,3],[9,2,8,4]]).
q([[5,6,4],[5,6,0],[3,3,2],[7,1,1,5]],[[5,6,4],[5,6,0],[5,5,4],[9,3,3,7]]).
q([[2,1,4,3],[5,0,4]],[[4,3,6,5],[5,0,4]]).
q([[6,6,2,7],[3,7,1,7]],[[6,6,2,7],[5,9,3,9]]).
q([[7,7,7,2],[1,1,2]],[[9,9,9,4],[1,1,2]]).
