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
my_last4(A,B):-last(A,B).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse8(A,B):-reverse(A,B).
my_element9(A,B):-member(B,A).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_msort12(A,B):-msort(A,B).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_set15(A):-list_to_set(A,A).
my_max_list16(A,B):-max_list(A,B).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set18(A,B):-list_to_set(A,B).
my_odd19(A):-1 is A mod 2.
my_pred20(A,B):-succ(B,A),A > 0.
my_sumlist21(A,B):-sumlist(A,B).
my_succ22(A,B):-succ(A,B),B =< 10.
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_uppercase6,[char]).
prim(my_toupper7,[char,char]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_element9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_tail11,[list(T),list(T)]).
prim(my_msort12,[list(int),list(int)]).
prim(my_min_list13,[list(int),int]).
prim(my_len14,[list(_),int]).
prim(my_set15,[list(_)]).
prim(my_max_list16,[list(int),int]).
prim(my_tolower17,[char,char]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_odd19,[int]).
prim(my_pred20,[int,int]).
prim(my_sumlist21,[list(int),int]).
prim(my_succ22,[int,int]).
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
p([2,7,3,3,9],[4]).
p([5,3,9,5,4,2],[8,4]).
p([4,4,1,2,4,2,2,7],[8,8,4,8,4,4]).
p([7,7,0,2,9],[0,4]).
p([9,5,1,4],[8]).
q([0,5,0,9,9],[0,0,9]).
q([2,1,1,2],[9,4,4]).
q([0,4,5,9],[0,8,7]).
q([7,4,5,7],[8,2]).
q([2,1,1,1,7,3,7],[5,4]).
