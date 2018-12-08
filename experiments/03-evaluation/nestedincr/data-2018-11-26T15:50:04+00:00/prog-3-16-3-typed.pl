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
my_double3(N,M):-M is 2*N,M =< 10.
my_list_to_set4(A,B):-list_to_set(A,B).
my_max_list5(A,B):-max_list(A,B).
my_head6([H|_],H).

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

my_sumlist8(A,B):-sumlist(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_tail10([_|TL],TL).
my_pred11(A,B):-succ(B,A),A > 0.
my_min_list12(A,B):-min_list(A,B).
my_set13(A):-list_to_set(A,A).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_last15(A,B):-last(A,B).
my_msort16(A,B):-msort(A,B).
my_odd17(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_max_list5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_tolower9,[char,char]).
prim(my_tail10,[list(T),list(T)]).
prim(my_pred11,[int,int]).
prim(my_min_list12,[list(int),int]).
prim(my_set13,[list(_)]).
prim(my_uppercase14,[char]).
prim(my_last15,[list(T),T]).
prim(my_msort16,[list(int),list(int)]).
prim(my_odd17,[int]).
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
p([[7,2,1],[5,6,3,3],[2,3,4,6],[5,7,5,1]],[[9,4,3],[7,8,5,5],[4,5,6,8],[7,9,7,3]]).
p([[5,6,3,6],[7,6,1,2],[7,2,7]],[[7,8,5,8],[9,8,3,4],[9,4,9]]).
p([[1,1,3,0],[2,2,2,0],[2,6,1],[3,7,0,0]],[[3,3,5,2],[4,4,4,2],[4,8,3],[5,9,2,2]]).
p([[4,1,6],[0,7,5]],[[6,3,8],[2,9,7]]).
p([[4,2,2,7],[0,1,1],[6,5,3,0],[0,6,6,5]],[[6,4,4,9],[2,3,3],[8,7,5,2],[2,8,8,7]]).
q([[3,6,7,4],[5,5,0,6],[5,1,6,4],[3,7,3]],[[5,8,9,6],[5,5,0,6],[5,1,6,4],[5,9,5]]).
q([[3,0,1],[7,7,4],[6,7,6],[1,2,5,1]],[[5,2,3],[9,9,6],[6,7,6],[1,2,5,1]]).
q([[0,1,4,7],[6,6,0,2],[3,1,6]],[[2,3,6,9],[8,8,2,4],[3,1,6]]).
q([[1,5,1],[2,0,4,0],[4,1,5],[3,0,1]],[[1,5,1],[4,2,6,2],[4,1,5],[5,2,3]]).
q([[4,6,0,3],[2,7,2,5],[3,3,1]],[[6,8,2,5],[4,9,4,7],[3,3,1]]).
