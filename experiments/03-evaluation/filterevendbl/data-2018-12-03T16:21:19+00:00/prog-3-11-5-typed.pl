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
my_reverse4(A,B):-reverse(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_set6(A):-list_to_set(A,A).
my_head7([H|_],H).
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist9(A,B):-sumlist(A,B).
my_max_list10(A,B):-max_list(A,B).
my_element11(A,B):-member(B,A).
my_uppercase12(A):-upcase_atom(A,A),char_code(A,_).
my_pred13(A,B):-succ(B,A),A > 0.
my_succ14(A,B):-succ(A,B),B =< 10.
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_set6,[list(_)]).
prim(my_head7,[list(T),T]).
prim(my_toupper8,[char,char]).
prim(my_sumlist9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_element11,[list(T),T]).
prim(my_uppercase12,[char]).
prim(my_pred13,[int,int]).
prim(my_succ14,[int,int]).
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
p([7,2,3,7,0,3,5],[4,0]).
p([4,3,0,1,2,4,0,2,4],[8,0,4,8,0,4,8]).
p([0,4,9,3,2,3],[0,8,4]).
p([9,5,0,2,4],[0,4,8]).
p([0,3,3,5,0,7],[0,0]).
q([9,1,9,5,0,5,7,9,1],[0,3]).
q([5,4,4,0,0,2,4],[0,4,8,8,0,1,8]).
q([7,1,2,4,2,4,5,0,4],[4,8,4,1,8,0,8]).
q([2,1,1,0,1,9],[4,1,0]).
q([2,5,7,7,1],[6,4]).
