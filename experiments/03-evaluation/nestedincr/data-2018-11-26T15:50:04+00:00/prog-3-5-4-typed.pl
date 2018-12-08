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
my_tail2([_|TL],TL).
my_element3(A,B):-member(B,A).

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

my_head5([H|_],H).
my_min_list6(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_tail2,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_head5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
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
p([[0,6,2],[6,7,2],[1,4,4]],[[2,8,4],[8,9,4],[3,6,6]]).
p([[2,4,6,3],[6,7,3]],[[4,6,8,5],[8,9,5]]).
p([[1,1,6,6],[1,7,3,7]],[[3,3,8,8],[3,9,5,9]]).
p([[5,1,7,1],[0,0,4,1]],[[7,3,9,3],[2,2,6,3]]).
p([[5,3,0,5],[4,7,5]],[[7,5,2,7],[6,9,7]]).
q([[4,2,4],[1,4,6],[3,4,3]],[[6,4,6],[3,6,8],[3,4,3]]).
q([[5,2,0,0],[2,4,3],[1,1,3,4],[1,0,3]],[[7,4,2,2],[2,4,3],[3,3,5,6],[3,2,5]]).
q([[4,2,2],[7,6,4],[1,3,2]],[[6,4,4],[7,6,4],[3,5,4]]).
q([[4,5,1,6],[0,6,3,3],[1,7,6]],[[4,5,1,6],[2,8,5,5],[3,9,8]]).
q([[5,3,7],[1,3,5]],[[5,3,7],[3,5,7]]).
