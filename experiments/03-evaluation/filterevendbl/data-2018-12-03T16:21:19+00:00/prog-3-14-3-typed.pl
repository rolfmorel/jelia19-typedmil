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
my_flatten4(A,B):-flatten(A,B).
my_msort5(A,B):-msort(A,B).
my_set6(A):-list_to_set(A,A).
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_pred8(A,B):-succ(B,A),A > 0.
my_last9(A,B):-last(A,B).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_succ11(A,B):-succ(A,B),B =< 10.
my_lowercase12(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist13(A,B):-sumlist(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_reverse17(A,B):-reverse(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_set6,[list(_)]).
prim(my_uppercase7,[char]).
prim(my_pred8,[int,int]).
prim(my_last9,[list(T),T]).
prim(my_tolower10,[char,char]).
prim(my_succ11,[int,int]).
prim(my_lowercase12,[char]).
prim(my_sumlist13,[list(int),int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_head15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_reverse17,[list(T),list(T)]).
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
p([7,4,0,4,4,3],[8,0,8,8]).
p([9,0,7,3,3],[0]).
p([7,9,4,1,5,3],[8]).
p([5,0,2,0,7,1,0,9,2],[0,4,0,0,4]).
p([0,7,0,9,2],[0,0,4]).
q([0,4,3,0,2,5,9,9],[9,0,8,0,4]).
q([7,1,4,2,2,0,0,9,9],[4,0,4,8,4,0]).
q([7,9,7,5,0,9,4],[0,3,8]).
q([3,3,0,9],[0,0]).
q([9,9,0,5],[3,0]).
