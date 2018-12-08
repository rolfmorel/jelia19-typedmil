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
my_element4(A,B):-member(B,A).
my_last5(A,B):-last(A,B).
my_set6(A):-list_to_set(A,A).
my_min_list7(A,B):-min_list(A,B).
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_head9([H|_],H).
my_reverse10(A,B):-reverse(A,B).
my_odd11(A):-1 is A mod 2.
my_succ12(A,B):-succ(A,B),B =< 10.
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_element4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_set6,[list(_)]).
prim(my_min_list7,[list(int),int]).
prim(my_toupper8,[char,char]).
prim(my_head9,[list(T),T]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_odd11,[int]).
prim(my_succ12,[int,int]).
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
p([9,9,5,7,3,3,7,3,2],[4]).
p([7,0,4,4,9,2],[0,8,8,4]).
p([3,5,9,0,0,4,0],[0,0,8,0]).
p([3,2,4,7,4,4],[4,8,8,8]).
p([7,2,7,5,3,7,1,0],[4,0]).
q([2,5,7,4,2,5,3,2,3],[4,9,8,4,4]).
q([2,7,0,2,2,0,0],[4,0,0,4,0,9,4]).
q([7,7,1,4,7],[8,2]).
q([3,0,7,9,3],[0,5]).
q([0,0,4,4,4,2,0,4,9],[8,8,5,0,0,0,8,4,8]).
