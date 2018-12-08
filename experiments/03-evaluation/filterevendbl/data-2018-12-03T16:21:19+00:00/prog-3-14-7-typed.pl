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
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_list_to_set6(A,B):-list_to_set(A,B).
my_set7(A):-list_to_set(A,A).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_tail9([_|TL],TL).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_reverse11(A,B):-reverse(A,B).
my_max_list12(A,B):-max_list(A,B).
my_odd13(A):-1 is A mod 2.
my_msort14(A,B):-msort(A,B).
my_toupper15(A,B):-upcase_atom(A,B),char_code(A,_).
my_element16(A,B):-member(B,A).
my_last17(A,B):-last(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_min_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_set7,[list(_)]).
prim(my_uppercase8,[char]).
prim(my_tail9,[list(T),list(T)]).
prim(my_tolower10,[char,char]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_max_list12,[list(int),int]).
prim(my_odd13,[int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_toupper15,[char,char]).
prim(my_element16,[list(T),T]).
prim(my_last17,[list(T),T]).
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
p([2,0,1,0,5,4,2,2,2],[4,0,0,8,4,4,4]).
p([1,2,7,1,1],[4]).
p([1,1,9,5,7,0,9,3,9],[0]).
p([1,3,4,2,5,2,0],[8,4,4,0]).
p([0,4,4,1],[0,8,8]).
q([0,2,2,4,0,0,4,7,5],[1,4,0,4,0,0,8,8]).
q([9,3,4,1,5,3,9,9,9],[8,1]).
q([4,4,1,4,2,0,2],[0,3,4,8,8,4,8]).
q([3,1,5,1,4],[8,8]).
q([4,3,4,1,3,3],[8,8,1]).
