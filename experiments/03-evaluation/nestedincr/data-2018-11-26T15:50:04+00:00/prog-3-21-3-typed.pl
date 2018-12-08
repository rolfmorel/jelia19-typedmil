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
my_last2(A,B):-last(A,B).
my_pred3(A,B):-succ(B,A),A > 0.
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_msort5(A,B):-msort(A,B).
my_element6(A,B):-member(B,A).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list8(A,B):-min_list(A,B).

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

my_list_to_set10(A,B):-list_to_set(A,B).
my_even11(A):-0 is A mod 2.
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_set15(A):-list_to_set(A,A).
my_head16([H|_],H).
my_tail17([_|TL],TL).
my_sumlist18(A,B):-sumlist(A,B).
my_flatten19(A,B):-flatten(A,B).
my_odd20(A):-1 is A mod 2.
my_double21(N,M):-M is 2*N,M =< 10.
my_toupper22(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_lowercase4,[char]).
prim(my_msort5,[list(int),list(int)]).
prim(my_element6,[list(T),T]).
prim(my_tolower7,[char,char]).
prim(my_min_list8,[list(int),int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_even11,[int]).
prim(my_max_list12,[list(int),int]).
prim(my_len13,[list(_),int]).
prim(my_uppercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_head16,[list(T),T]).
prim(my_tail17,[list(T),list(T)]).
prim(my_sumlist18,[list(int),int]).
prim(my_flatten19,[list(list(T)),list(T)]).
prim(my_odd20,[int]).
prim(my_double21,[int,int]).
prim(my_toupper22,[char,char]).
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
p([[3,4,1],[5,3,2],[0,3,3,2],[2,5,2,2]],[[5,6,3],[7,5,4],[2,5,5,4],[4,7,4,4]]).
p([[3,6,5],[3,2,6],[2,3,4],[7,6,4]],[[5,8,7],[5,4,8],[4,5,6],[9,8,6]]).
p([[0,4,7,4],[6,6,6],[0,4,4,5],[1,5,2,5]],[[2,6,9,6],[8,8,8],[2,6,6,7],[3,7,4,7]]).
p([[7,4,4,7],[2,5,2,4],[5,2,3,7]],[[9,6,6,9],[4,7,4,6],[7,4,5,9]]).
p([[2,7,1],[2,3,2],[3,5,3,1],[3,1,2]],[[4,9,3],[4,5,4],[5,7,5,3],[5,3,4]]).
q([[1,6,6,0],[7,0,6,3]],[[1,6,6,0],[9,2,8,5]]).
q([[3,2,3,1],[0,1,4,5],[4,4,4,4],[7,1,7,7]],[[5,4,5,3],[0,1,4,5],[6,6,6,6],[9,3,9,9]]).
q([[2,1,0,6],[0,4,3]],[[2,1,0,6],[2,6,5]]).
q([[1,5,1],[1,0,5],[4,6,6,1]],[[3,7,3],[3,2,7],[4,6,6,1]]).
q([[4,7,1,7],[4,6,7]],[[4,7,1,7],[6,8,9]]).
