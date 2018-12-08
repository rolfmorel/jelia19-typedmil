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
my_flatten5(A,B):-flatten(A,B).
my_msort6(A,B):-msort(A,B).
my_min_list7(A,B):-min_list(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_head9([H|_],H).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_succ11(A,B):-succ(A,B),B =< 10.
my_pred12(A,B):-succ(B,A),A > 0.
my_list_to_set13(A,B):-list_to_set(A,B).
my_tolower14(A,B):-downcase_atom(A,B),char_code(A,_).
my_reverse15(A,B):-reverse(A,B).
my_uppercase16(A):-upcase_atom(A,A),char_code(A,_).
my_tail17([_|TL],TL).
my_odd18(A):-1 is A mod 2.
my_set19(A):-list_to_set(A,A).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_element4,[list(T),T]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_msort6,[list(int),list(int)]).
prim(my_min_list7,[list(int),int]).
prim(my_lowercase8,[char]).
prim(my_head9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_succ11,[int,int]).
prim(my_pred12,[int,int]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_tolower14,[char,char]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_uppercase16,[char]).
prim(my_tail17,[list(T),list(T)]).
prim(my_odd18,[int]).
prim(my_set19,[list(_)]).
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
p([5,5,2,5,0],[4,0]).
p([4,0,3,7],[8,0]).
p([5,7,7,7,4,2,3,5],[8,4]).
p([1,3,3,5,1,1,5,0,5],[0]).
p([4,2,0,0,7],[8,4,0,0]).
q([4,5,4,2,4,3,0],[0,8,8,4,0,8]).
q([0,9,0,5,9,7,2,3],[0,0,4,1]).
q([2,0,0,2,1,4,7],[0,4,8,0,8,4]).
q([1,3,3,1,2,5,7,9,7],[0,4]).
q([1,1,1,1,4,7,2,7,9],[4,9,8]).