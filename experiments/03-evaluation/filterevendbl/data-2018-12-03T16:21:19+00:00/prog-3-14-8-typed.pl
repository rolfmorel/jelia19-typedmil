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
my_set4(A):-list_to_set(A,A).
my_min_list5(A,B):-min_list(A,B).
my_odd6(A):-1 is A mod 2.
my_max_list7(A,B):-max_list(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_len9(A,B):-length(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_element12(A,B):-member(B,A).
my_last13(A,B):-last(A,B).
my_flatten14(A,B):-flatten(A,B).
my_toupper15(A,B):-upcase_atom(A,B),char_code(A,_).
my_head16([H|_],H).
my_sumlist17(A,B):-sumlist(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_set4,[list(_)]).
prim(my_min_list5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_max_list7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_len9,[list(_),int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_uppercase11,[char]).
prim(my_element12,[list(T),T]).
prim(my_last13,[list(T),T]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_toupper15,[char,char]).
prim(my_head16,[list(T),T]).
prim(my_sumlist17,[list(int),int]).
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
p([0,0,2,2,4,0,4,1],[0,0,4,4,8,0,8]).
p([4,2,0,9,0,2,4],[8,4,0,0,4,8]).
p([4,2,7,2],[8,4,4]).
p([4,2,7,5,9,2,4,0],[8,4,4,8,0]).
p([4,2,0,0,9,4,2],[8,4,0,0,8,4]).
q([7,7,1,5,4],[8,2]).
q([5,4,7,1,5,3,7,5,5],[8,3]).
q([0,2,9,4],[0,4,0,8]).
q([0,2,5,4,2],[6,8,0,4,4]).
q([9,1,2,9,4],[8,7,4]).
