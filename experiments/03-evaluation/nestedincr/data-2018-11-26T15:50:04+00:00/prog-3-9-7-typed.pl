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
my_even3(A):-0 is A mod 2.
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_last6(A,B):-last(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_pred8(A,B):-succ(B,A),A > 0.
my_flatten9(A,B):-flatten(A,B).
my_max_list10(A,B):-max_list(A,B).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_even3,[int]).
prim(my_uppercase4,[char]).
prim(my_tolower5,[char,char]).
prim(my_last6,[list(T),T]).
prim(my_toupper7,[char,char]).
prim(my_pred8,[int,int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_max_list10,[list(int),int]).
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
p([[5,7,6,7],[0,6,5],[3,0,0,1]],[[7,9,8,9],[2,8,7],[5,2,2,3]]).
p([[5,4,3,7],[3,4,0]],[[7,6,5,9],[5,6,2]]).
p([[5,0,4,0],[2,4,4,0]],[[7,2,6,2],[4,6,6,2]]).
p([[2,5,0],[0,6,3]],[[4,7,2],[2,8,5]]).
p([[0,4,7,5],[2,4,6],[7,0,1,2]],[[2,6,9,7],[4,6,8],[9,2,3,4]]).
q([[3,6,3,3],[2,1,6],[6,7,1],[5,4,7,6]],[[5,8,5,5],[4,3,8],[6,7,1],[5,4,7,6]]).
q([[4,4,7],[5,4,1,4]],[[6,6,9],[5,4,1,4]]).
q([[6,4,2,2],[2,2,6,2]],[[8,6,4,4],[2,2,6,2]]).
q([[3,0,7],[0,0,0,5]],[[5,2,9],[0,0,0,5]]).
q([[1,7,1,1],[0,3,1],[0,1,5,7],[0,3,3,0]],[[3,9,3,3],[2,5,3],[0,1,5,7],[0,3,3,0]]).
