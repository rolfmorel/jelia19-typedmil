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
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_odd7(A):-1 is A mod 2.
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_element10(A,B):-member(B,A).
my_min_list11(A,B):-min_list(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_len5,[list(_),int]).
prim(my_uppercase6,[char]).
prim(my_odd7,[int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_succ9,[int,int]).
prim(my_element10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
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
p([0,4,0,2,1,0,4,7,7],[0,8,0,4,0,8]).
p([0,4,2,9,4,0,9,0,5],[0,8,4,8,0,0]).
p([2,7,0,0,1,9,5,4,5],[4,0,0,8]).
p([3,0,0,2,2,5,0],[0,0,4,4,0]).
p([0,2,3,2,4,2,0,4],[0,4,4,8,4,0,8]).
q([4,3,1,5],[9,8]).
q([9,2,4,2,0,0,4,0,2],[0,4,0,4,0,8,8,4,0]).
q([0,0,0,0,4,0,9],[0,0,8,8,0,0,0]).
q([5,0,7,5,7,3],[0,1]).
q([4,2,4,2,7],[4,8,8,4,4]).
