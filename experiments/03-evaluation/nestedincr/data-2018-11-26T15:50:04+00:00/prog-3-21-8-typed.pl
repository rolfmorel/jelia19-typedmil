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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_element5(A,B):-member(B,A).
my_len6(A,B):-length(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list8(A,B):-max_list(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort10(A,B):-msort(A,B).
my_odd11(A):-1 is A mod 2.
my_min_list12(A,B):-min_list(A,B).
my_set13(A):-list_to_set(A,A).
my_list_to_set14(A,B):-list_to_set(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_last19(A,B):-last(A,B).
my_even20(A):-0 is A mod 2.

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

my_flatten22(A,B):-flatten(A,B).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_lowercase3,[char]).
prim(my_pred4,[int,int]).
prim(my_element5,[list(T),T]).
prim(my_len6,[list(_),int]).
prim(my_toupper7,[char,char]).
prim(my_max_list8,[list(int),int]).
prim(my_tolower9,[char,char]).
prim(my_msort10,[list(int),list(int)]).
prim(my_odd11,[int]).
prim(my_min_list12,[list(int),int]).
prim(my_set13,[list(_)]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_double15,[int,int]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_last19,[list(T),T]).
prim(my_even20,[int]).
prim(my_flatten22,[list(list(T)),list(T)]).
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
p([[1,3,4],[1,7,4,3],[7,0,1],[7,1,2]],[[3,5,6],[3,9,6,5],[9,2,3],[9,3,4]]).
p([[5,1,5],[6,4,0],[0,3,6],[4,4,6,3]],[[7,3,7],[8,6,2],[2,5,8],[6,6,8,5]]).
p([[7,1,5],[6,4,0,1],[3,3,0,3]],[[9,3,7],[8,6,2,3],[5,5,2,5]]).
p([[5,4,0],[5,1,7,4]],[[7,6,2],[7,3,9,6]]).
p([[3,2,1,5],[3,4,6,1],[0,0,1,6],[0,7,2,0]],[[5,4,3,7],[5,6,8,3],[2,2,3,8],[2,9,4,2]]).
q([[7,1,1],[1,7,5,5],[1,1,0,1],[4,7,1]],[[7,1,1],[3,9,7,7],[3,3,2,3],[6,9,3]]).
q([[1,3,0,6],[3,5,4,3]],[[3,5,2,8],[3,5,4,3]]).
q([[4,0,1],[7,0,2,0],[7,0,6]],[[6,2,3],[9,2,4,2],[7,0,6]]).
q([[6,1,2,4],[0,4,5,6]],[[6,1,2,4],[2,6,7,8]]).
q([[7,0,4],[0,3,1]],[[7,0,4],[2,5,3]]).
