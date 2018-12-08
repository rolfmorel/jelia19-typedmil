:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_reverse2(A,B):-reverse(A,B).
my_odd3(A):-1 is A mod 2.
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_head6([H|_],H).
my_double7(N,M):-M is 2*N,M =< 10.
my_tail8([_|TL],TL).
my_max_list9(A,B):-max_list(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_flatten12(A,B):-flatten(A,B).
my_even13(A):-0 is A mod 2.
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_last15(A,B):-last(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_len17(A,B):-length(A,B).
my_msort18(A,B):-msort(A,B).
my_toupper19(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_odd3/1).
prim(my_tolower4/2).
prim(my_head6/2).
prim(my_double7/2).
prim(my_tail8/2).
prim(my_max_list9/2).
prim(my_sumlist10/2).
prim(my_list_to_set11/2).
prim(my_flatten12/2).
prim(my_even13/1).
prim(my_uppercase14/1).
prim(my_last15/2).
prim(my_pred16/2).
prim(my_len17/2).
prim(my_msort18/2).
prim(my_toupper19/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([[5,5,7,1],[2,6,2,3],[0,3,5,6],[0,5,0]],[[7,7,9,3],[4,8,4,5],[2,5,7,8],[2,7,2]]).
p([[6,6,0,0],[6,3,1,6],[7,2,5],[7,1,4]],[[8,8,2,2],[8,5,3,8],[9,4,7],[9,3,6]]).
p([[1,4,0],[4,5,1],[1,7,3],[4,3,0,1]],[[3,6,2],[6,7,3],[3,9,5],[6,5,2,3]]).
p([[3,7,7],[0,7,5],[5,1,4,7],[5,2,0]],[[5,9,9],[2,9,7],[7,3,6,9],[7,4,2]]).
p([[5,2,4],[4,5,1,7],[1,1,3,1],[4,4,2,7]],[[7,4,6],[6,7,3,9],[3,3,5,3],[6,6,4,9]]).
q([[4,4,5,2],[6,5,1]],[[4,4,5,2],[8,7,3]]).
q([[3,0,6],[1,5,2],[6,4,1,1]],[[5,2,8],[3,7,4],[6,4,1,1]]).
q([[7,3,5],[3,3,1,6],[6,0,0,0],[3,1,6,1]],[[7,3,5],[5,5,3,8],[8,2,2,2],[3,1,6,1]]).
q([[2,3,4,0],[7,5,3],[5,2,2,4]],[[2,3,4,0],[9,7,5],[7,4,4,6]]).
q([[0,4,0,0],[0,3,1],[6,0,1,7],[6,4,1,7]],[[2,6,2,2],[0,3,1],[6,0,1,7],[8,6,3,9]]).
