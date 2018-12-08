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
my_len3(A,B):-length(A,B).

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

my_set5(A):-list_to_set(A,A).
my_max_list6(A,B):-max_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_odd8(A):-1 is A mod 2.
my_tail9([_|TL],TL).
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_reverse13(A,B):-reverse(A,B).
prim(my_succ1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_len3,[list(_),int]).
prim(my_set5,[list(_)]).
prim(my_max_list6,[list(int),int]).
prim(my_min_list7,[list(int),int]).
prim(my_odd8,[int]).
prim(my_tail9,[list(T),list(T)]).
prim(my_even10,[int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_reverse13,[list(T),list(T)]).
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
p([[1,5,1],[5,3,5,4]],[[3,7,3],[7,5,7,6]]).
p([[0,3,7],[0,3,0,0]],[[2,5,9],[2,5,2,2]]).
p([[6,1,2,6],[6,4,0]],[[8,3,4,8],[8,6,2]]).
p([[6,6,1,3],[7,1,0,7],[2,4,7]],[[8,8,3,5],[9,3,2,9],[4,6,9]]).
p([[4,4,5,3],[1,1,7],[7,3,4,6]],[[6,6,7,5],[3,3,9],[9,5,6,8]]).
q([[3,3,1],[5,4,2],[6,2,2,4]],[[3,3,1],[7,6,4],[8,4,4,6]]).
q([[0,6,5],[3,7,0],[0,4,1]],[[0,6,5],[5,9,2],[2,6,3]]).
q([[7,5,2,3],[0,7,3,4]],[[9,7,4,5],[0,7,3,4]]).
q([[1,1,5],[6,7,6],[2,1,2],[6,3,1]],[[1,1,5],[8,9,8],[4,3,4],[6,3,1]]).
q([[7,0,6,6],[5,7,7,5],[1,7,6]],[[9,2,8,8],[7,9,9,7],[1,7,6]]).
