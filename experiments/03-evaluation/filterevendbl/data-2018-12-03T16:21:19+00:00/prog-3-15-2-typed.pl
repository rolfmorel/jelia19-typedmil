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
my_odd4(A):-1 is A mod 2.
my_max_list5(A,B):-max_list(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set8(A,B):-list_to_set(A,B).
my_msort9(A,B):-msort(A,B).
my_head10([H|_],H).
my_set11(A):-list_to_set(A,A).
my_pred12(A,B):-succ(B,A),A > 0.
my_tail13([_|TL],TL).
my_reverse14(A,B):-reverse(A,B).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist16(A,B):-sumlist(A,B).
my_element17(A,B):-member(B,A).
my_last18(A,B):-last(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_odd4,[int]).
prim(my_max_list5,[list(int),int]).
prim(my_uppercase6,[char]).
prim(my_lowercase7,[char]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_msort9,[list(int),list(int)]).
prim(my_head10,[list(T),T]).
prim(my_set11,[list(_)]).
prim(my_pred12,[int,int]).
prim(my_tail13,[list(T),list(T)]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_tolower15,[char,char]).
prim(my_sumlist16,[list(int),int]).
prim(my_element17,[list(T),T]).
prim(my_last18,[list(T),T]).
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
p([7,2,2,1],[4,4]).
p([7,1,0,1,7,1,0,1],[0,0]).
p([5,5,3,0,7],[0]).
p([0,9,0,4,4,0,4,2],[0,0,8,8,0,8,4]).
p([0,3,0,2],[0,0,4]).
q([9,1,7,4,4,0,4],[6,0,8,8,8]).
q([4,0,7,0,5,0],[0,6,0,0,8]).
q([7,5,9,1,2,9,5,5,4],[8,8,4]).
q([5,1,9,9,0,1,7,9,9],[0,5]).
q([0,0,4,2,1,0,7,0,2],[0,7,0,8,4,0,0,4]).