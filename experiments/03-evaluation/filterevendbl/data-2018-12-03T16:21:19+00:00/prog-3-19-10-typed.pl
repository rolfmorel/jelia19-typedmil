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
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_pred7(A,B):-succ(B,A),A > 0.
my_max_list8(A,B):-max_list(A,B).
my_tail9([_|TL],TL).
my_list_to_set10(A,B):-list_to_set(A,B).
my_element11(A,B):-member(B,A).
my_len12(A,B):-length(A,B).
my_succ13(A,B):-succ(A,B),B =< 10.
my_lowercase14(A):-downcase_atom(A,A),char_code(A,_).
my_head15([H|_],H).
my_last16(A,B):-last(A,B).
my_msort17(A,B):-msort(A,B).
my_reverse18(A,B):-reverse(A,B).
my_tolower19(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten20(A,B):-flatten(A,B).
my_toupper21(A,B):-upcase_atom(A,B),char_code(A,_).
my_odd22(A):-1 is A mod 2.
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_sumlist4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_uppercase6,[char]).
prim(my_pred7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_tail9,[list(T),list(T)]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_element11,[list(T),T]).
prim(my_len12,[list(_),int]).
prim(my_succ13,[int,int]).
prim(my_lowercase14,[char]).
prim(my_head15,[list(T),T]).
prim(my_last16,[list(T),T]).
prim(my_msort17,[list(int),list(int)]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_tolower19,[char,char]).
prim(my_flatten20,[list(list(T)),list(T)]).
prim(my_toupper21,[char,char]).
prim(my_odd22,[int]).
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
p([3,2,0,2,4,2,4],[4,0,4,8,4,8]).
p([7,0,7,7],[0]).
p([1,7,7,1,0,4],[0,8]).
p([2,1,7,3],[4]).
p([0,7,2,4,7],[0,4,8]).
q([3,2,9,3,5,1],[4,3]).
q([3,4,1,4,2],[4,7,8,8]).
q([0,9,4,3,5,2],[9,4,0,8]).
q([4,0,2,5,0,3,4],[8,4,0,8,2,0]).
q([7,0,4,4,2,2],[8,6,4,8,4,0]).
