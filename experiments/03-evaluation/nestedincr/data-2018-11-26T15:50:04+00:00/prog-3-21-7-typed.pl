:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_set2(A):-list_to_set(A,A).
my_flatten3(A,B):-flatten(A,B).
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).

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

my_head9([H|_],H).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list11(A,B):-max_list(A,B).
my_element12(A,B):-member(B,A).
my_len13(A,B):-length(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_last16(A,B):-last(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_odd18(A):-1 is A mod 2.
my_sumlist19(A,B):-sumlist(A,B).
my_pred20(A,B):-succ(B,A),A > 0.
my_msort21(A,B):-msort(A,B).
my_even22(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_set2,[list(_)]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_tolower4,[char,char]).
prim(my_uppercase5,[char]).
prim(my_min_list6,[list(int),int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_head9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_max_list11,[list(int),int]).
prim(my_element12,[list(T),T]).
prim(my_len13,[list(_),int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_lowercase15,[char]).
prim(my_last16,[list(T),T]).
prim(my_double17,[int,int]).
prim(my_odd18,[int]).
prim(my_sumlist19,[list(int),int]).
prim(my_pred20,[int,int]).
prim(my_msort21,[list(int),list(int)]).
prim(my_even22,[int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(int)),list(list(int))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([[2,7,4],[5,6,5,3]],[[4,9,6],[7,8,7,5]]).
p([[2,4,3],[4,6,7],[7,2,1]],[[4,6,5],[6,8,9],[9,4,3]]).
p([[3,3,7,3],[3,5,5]],[[5,5,9,5],[5,7,7]]).
p([[1,0,4,7],[0,3,3],[1,0,3,4]],[[3,2,6,9],[2,5,5],[3,2,5,6]]).
p([[3,0,6,1],[1,7,1]],[[5,2,8,3],[3,9,3]]).
q([[2,6,5],[2,4,4]],[[2,6,5],[4,6,6]]).
q([[2,2,3],[6,1,7,3]],[[2,2,3],[8,3,9,5]]).
q([[5,3,2,3],[6,5,4,2]],[[7,5,4,5],[6,5,4,2]]).
q([[0,3,5,1],[3,7,4,1],[0,2,2],[5,5,3]],[[0,3,5,1],[3,7,4,1],[2,4,4],[7,7,5]]).
q([[1,2,2,7],[3,3,4],[5,5,4,3],[3,0,4]],[[3,4,4,9],[3,3,4],[7,7,6,5],[3,0,4]]).
