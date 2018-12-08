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
my_even3(A):-0 is A mod 2.
my_set4(A):-list_to_set(A,A).
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_odd7(A):-1 is A mod 2.
my_last8(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_tail2,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_set4,[list(_)]).
prim(my_tolower5,[char,char]).
prim(my_odd7,[int]).
prim(my_last8,[list(T),T]).
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
p([[2,4,2],[5,4,4,4]],[[4,6,4],[7,6,6,6]]).
p([[2,4,5,2],[6,5,0],[4,4,7],[2,4,4,2]],[[4,6,7,4],[8,7,2],[6,6,9],[4,6,6,4]]).
p([[2,0,1],[2,4,7],[4,4,0]],[[4,2,3],[4,6,9],[6,6,2]]).
p([[1,0,5],[6,4,7,1],[5,6,2],[3,0,6,5]],[[3,2,7],[8,6,9,3],[7,8,4],[5,2,8,7]]).
p([[0,1,3,7],[4,4,3],[7,7,3],[4,3,6]],[[2,3,5,9],[6,6,5],[9,9,5],[6,5,8]]).
q([[3,5,7,2],[0,2,2],[6,7,3,4]],[[3,5,7,2],[2,4,4],[8,9,5,6]]).
q([[0,0,7],[4,2,2,4],[6,7,6]],[[2,2,9],[6,4,4,6],[6,7,6]]).
q([[1,4,7],[1,1,1,4]],[[1,4,7],[3,3,3,6]]).
q([[1,5,6],[6,1,2,2]],[[3,7,8],[6,1,2,2]]).
q([[6,2,4],[7,0,0],[1,2,7],[7,0,2]],[[8,4,6],[9,2,2],[1,2,7],[9,2,4]]).
