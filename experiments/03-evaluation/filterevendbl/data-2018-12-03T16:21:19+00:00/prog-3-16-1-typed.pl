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
my_msort4(A,B):-msort(A,B).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_len12(A,B):-length(A,B).
my_odd13(A):-1 is A mod 2.
my_sumlist14(A,B):-sumlist(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
my_element17(A,B):-member(B,A).
my_set18(A):-list_to_set(A,A).
my_list_to_set19(A,B):-list_to_set(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_last5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_max_list8,[list(int),int]).
prim(my_succ9,[int,int]).
prim(my_uppercase10,[char]).
prim(my_toupper11,[char,char]).
prim(my_len12,[list(_),int]).
prim(my_odd13,[int]).
prim(my_sumlist14,[list(int),int]).
prim(my_pred15,[int,int]).
prim(my_lowercase16,[char]).
prim(my_element17,[list(T),T]).
prim(my_set18,[list(_)]).
prim(my_list_to_set19,[list(T),list(T)]).
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
p([0,0,0,2,3],[0,0,0,4]).
p([2,5,7,7,0,2,2],[4,0,4,4]).
p([0,2,4,3,2,0],[0,4,8,4,0]).
p([5,1,5,2,4],[4,8]).
p([1,4,1,4,9],[8,8]).
q([5,4,0,1,3,0],[0,7,8,0]).
q([1,4,7,5],[8,0]).
q([5,0,1,0,2,3],[9,0,4,0]).
q([5,7,9,0,4,4,3,2],[4,0,8,8,5]).
q([7,4,5,4,7],[0,8,8]).
