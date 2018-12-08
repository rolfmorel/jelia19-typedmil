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
my_sumlist3(A,B):-sumlist(A,B).
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse5(A,B):-reverse(A,B).
my_last6(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_sumlist3,[list(int),int]).
prim(my_toupper4,[char,char]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_last6,[list(T),T]).
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
p([[6,6,6],[3,0,7,0],[1,3,5,2]],[[8,8,8],[5,2,9,2],[3,5,7,4]]).
p([[4,1,2],[2,3,5,7],[4,0,1]],[[6,3,4],[4,5,7,9],[6,2,3]]).
p([[3,4,6,5],[3,5,3,2],[2,1,4,3]],[[5,6,8,7],[5,7,5,4],[4,3,6,5]]).
p([[5,5,7],[3,4,4,5],[7,0,6]],[[7,7,9],[5,6,6,7],[9,2,8]]).
p([[3,4,4,3],[5,4,2],[0,7,3],[4,5,4,3]],[[5,6,6,5],[7,6,4],[2,9,5],[6,7,6,5]]).
q([[7,5,3,2],[1,7,0,2],[3,5,0,4],[3,3,0,3]],[[9,7,5,4],[1,7,0,2],[5,7,2,6],[3,3,0,3]]).
q([[3,5,5,0],[2,0,0,7],[6,4,7,5]],[[5,7,7,2],[4,2,2,9],[6,4,7,5]]).
q([[2,5,4],[0,7,6,4],[1,1,5,4],[2,3,7]],[[4,7,6],[2,9,8,6],[1,1,5,4],[4,5,9]]).
q([[3,6,1],[6,3,5,1],[5,1,2,4]],[[5,8,3],[6,3,5,1],[7,3,4,6]]).
q([[2,0,7],[1,7,4,3],[5,3,7]],[[4,2,9],[3,9,6,5],[5,3,7]]).
