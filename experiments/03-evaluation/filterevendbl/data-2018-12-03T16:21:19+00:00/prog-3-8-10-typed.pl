:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_even2(A):-0 is A mod 2.
my_double3(N,M):-M is 2*N,M =< 10.
my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_tail7([_|TL],TL).
my_list_to_set8(A,B):-list_to_set(A,B).
my_flatten9(A,B):-flatten(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_len11(A,B):-length(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_sumlist4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_tail7,[list(T),list(T)]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_succ10,[int,int]).
prim(my_len11,[list(_),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(int),list(int)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([1,0,3,7,3,9],[0]).
p([0,9,0,0,4],[0,0,0,8]).
p([5,2,7,2,0,1,4,7,4],[4,4,0,8,8]).
p([3,2,0,0,2],[4,0,0,4]).
p([2,7,0,2,2,0,2],[4,0,4,4,0,4]).
q([3,2,2,0],[4,4,4,0]).
q([1,5,4,9,7,0,9,7],[8,3,0]).
q([0,7,3,9],[0,2]).
q([1,2,0,3,4,3],[4,6,8,0]).
q([0,1,4,4,5,2,3,4],[4,0,0,8,8,8]).
