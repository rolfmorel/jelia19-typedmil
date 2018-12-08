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
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_set9(A):-list_to_set(A,A).
my_min_list10(A,B):-min_list(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_succ12(A,B):-succ(A,B),B =< 10.
my_reverse13(A,B):-reverse(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set16(A,B):-list_to_set(A,B).
my_len17(A,B):-length(A,B).
my_msort18(A,B):-msort(A,B).
my_element19(A,B):-member(B,A).
my_lowercase20(A):-downcase_atom(A,A),char_code(A,_).
my_tolower21(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),list(T)]).
prim(my_max_list8,[list(int),int]).
prim(my_set9,[list(_)]).
prim(my_min_list10,[list(int),int]).
prim(my_toupper11,[char,char]).
prim(my_succ12,[int,int]).
prim(my_reverse13,[list(T),list(T)]).
prim(my_pred14,[int,int]).
prim(my_uppercase15,[char]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_len17,[list(_),int]).
prim(my_msort18,[list(int),list(int)]).
prim(my_element19,[list(T),T]).
prim(my_lowercase20,[char]).
prim(my_tolower21,[char,char]).
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
p([2,1,2,0,5,2,0,7],[4,4,0,4,0]).
p([7,4,9,0],[8,0]).
p([0,2,3,2],[0,4,4]).
p([2,0,5,4,2,2,4,4],[4,0,8,4,4,8,8]).
p([4,4,1,3,5],[8,8]).
q([2,3,4,0],[8,4,0,7]).
q([9,9,1,3,9,4,9],[6,8]).
q([7,4,0,0,4,4,0],[8,8,6,0,0,0,8]).
q([4,9,5,1,5,7,4,3,1],[6,8,8]).
q([0,7,0,0,2,4,5],[5,0,0,4,8,0]).
