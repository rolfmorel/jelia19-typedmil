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
my_flatten2(A,B):-flatten(A,B).
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_even5(A):-0 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_head11([H|_],H).

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

my_tail13([_|TL],TL).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_max_list15(A,B):-max_list(A,B).
my_odd16(A):-1 is A mod 2.
my_reverse17(A,B):-reverse(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_element19(A,B):-member(B,A).
my_pred20(A,B):-succ(B,A),A > 0.
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_tolower3,[char,char]).
prim(my_double4,[int,int]).
prim(my_even5,[int]).
prim(my_set6,[list(_)]).
prim(my_lowercase7,[char]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_head11,[list(T),T]).
prim(my_tail13,[list(T),list(T)]).
prim(my_uppercase14,[char]).
prim(my_max_list15,[list(int),int]).
prim(my_odd16,[int]).
prim(my_reverse17,[list(T),list(T)]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_element19,[list(T),T]).
prim(my_pred20,[int,int]).
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
p([[2,2,2,4],[0,5,6,5],[7,6,1]],[[4,4,4,6],[2,7,8,7],[9,8,3]]).
p([[2,3,3,2],[2,7,5,3]],[[4,5,5,4],[4,9,7,5]]).
p([[2,7,1,2],[0,6,5,7]],[[4,9,3,4],[2,8,7,9]]).
p([[4,4,7],[7,2,6],[2,4,4],[2,4,2]],[[6,6,9],[9,4,8],[4,6,6],[4,6,4]]).
p([[3,1,3],[3,2,2,7]],[[5,3,5],[5,4,4,9]]).
q([[7,2,6],[2,2,5]],[[7,2,6],[4,4,7]]).
q([[1,1,1,1],[6,7,6,0]],[[3,3,3,3],[6,7,6,0]]).
q([[4,2,3,0],[0,6,1,5],[1,5,1],[2,2,2]],[[6,4,5,2],[2,8,3,7],[1,5,1],[2,2,2]]).
q([[6,2,7,0],[1,4,0]],[[6,2,7,0],[3,6,2]]).
q([[1,6,5,0],[1,5,2,5]],[[1,6,5,0],[3,7,4,7]]).
