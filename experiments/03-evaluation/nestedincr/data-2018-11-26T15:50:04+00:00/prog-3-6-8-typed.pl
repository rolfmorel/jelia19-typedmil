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
my_flatten2(A,B):-flatten(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_even7(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_max_list4,[list(int),int]).
prim(my_tail5,[list(T),list(T)]).
prim(my_last6,[list(T),T]).
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
p([[3,6,0,2],[6,5,4,5],[6,5,2,3]],[[5,8,2,4],[8,7,6,7],[8,7,4,5]]).
p([[1,0,2,6],[2,5,5,5],[2,2,1],[7,3,2]],[[3,2,4,8],[4,7,7,7],[4,4,3],[9,5,4]]).
p([[4,3,2,3],[3,4,0,0]],[[6,5,4,5],[5,6,2,2]]).
p([[3,2,5],[4,0,2],[6,7,1]],[[5,4,7],[6,2,4],[8,9,3]]).
p([[6,3,5,5],[0,4,2,4],[2,6,3]],[[8,5,7,7],[2,6,4,6],[4,8,5]]).
q([[6,3,1],[4,6,3,0],[4,1,4]],[[8,5,3],[4,6,3,0],[6,3,6]]).
q([[5,5,3,3],[5,2,5],[4,3,6,2]],[[7,7,5,5],[5,2,5],[6,5,8,4]]).
q([[7,2,3],[0,5,1]],[[9,4,5],[0,5,1]]).
q([[3,5,0],[1,0,6]],[[3,5,0],[3,2,8]]).
q([[5,7,7],[7,1,6,1],[5,7,0,6],[6,7,3,6]],[[5,7,7],[9,3,8,3],[7,9,2,8],[8,9,5,8]]).
