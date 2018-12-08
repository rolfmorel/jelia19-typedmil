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

my_last3(A,B):-last(A,B).
my_head4([H|_],H).
my_double5(N,M):-M is 2*N,M =< 10.
my_odd6(A):-1 is A mod 2.
my_list_to_set7(A,B):-list_to_set(A,B).
my_set8(A):-list_to_set(A,A).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_msort12(A,B):-msort(A,B).
my_tail13([_|TL],TL).
prim(my_succ1,[int,int]).
prim(my_last3,[list(T),T]).
prim(my_head4,[list(T),T]).
prim(my_double5,[int,int]).
prim(my_odd6,[int]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_set8,[list(_)]).
prim(my_sumlist9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_toupper11,[char,char]).
prim(my_msort12,[list(int),list(int)]).
prim(my_tail13,[list(T),list(T)]).
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
p([[5,5,5],[1,5,4]],[[7,7,7],[3,7,6]]).
p([[4,0,0],[2,4,1],[0,0,6],[6,4,4]],[[6,2,2],[4,6,3],[2,2,8],[8,6,6]]).
p([[5,2,5],[3,5,7],[5,1,4,2],[6,0,2]],[[7,4,7],[5,7,9],[7,3,6,4],[8,2,4]]).
p([[1,7,3,2],[6,1,4],[6,2,2]],[[3,9,5,4],[8,3,6],[8,4,4]]).
p([[2,5,3,5],[0,1,1]],[[4,7,5,7],[2,3,3]]).
q([[7,3,1],[0,1,7],[2,7,3,6],[2,4,7,5]],[[9,5,3],[0,1,7],[4,9,5,8],[4,6,9,7]]).
q([[4,6,1,4],[6,2,0],[7,3,4,7]],[[6,8,3,6],[6,2,0],[9,5,6,9]]).
q([[0,6,6],[6,2,5],[2,3,4],[4,4,0,3]],[[0,6,6],[8,4,7],[4,5,6],[6,6,2,5]]).
q([[7,1,0,7],[1,0,0,1],[3,2,4]],[[9,3,2,9],[3,2,2,3],[3,2,4]]).
q([[1,6,0],[7,7,6],[7,2,0,2],[0,4,7,4]],[[3,8,2],[9,9,8],[9,4,2,4],[0,4,7,4]]).
