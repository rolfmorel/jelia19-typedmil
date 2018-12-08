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
my_sumlist2(A,B):-sumlist(A,B).
my_odd3(A):-1 is A mod 2.
my_list_to_set4(A,B):-list_to_set(A,B).
my_flatten5(A,B):-flatten(A,B).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_msort12(A,B):-msort(A,B).
my_last13(A,B):-last(A,B).
my_reverse14(A,B):-reverse(A,B).

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

prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_odd3/1).
prim(my_list_to_set4/2).
prim(my_flatten5/2).
prim(my_toupper6/2).
prim(my_set7/1).
prim(my_element8/2).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_uppercase11/1).
prim(my_msort12/2).
prim(my_last13/2).
prim(my_reverse14/2).
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
p([[0,0,7],[4,3,1,7],[1,5,1,2],[7,5,2]],[[2,2,9],[6,5,3,9],[3,7,3,4],[9,7,4]]).
p([[6,6,6],[4,0,0,5]],[[8,8,8],[6,2,2,7]]).
p([[4,1,3,7],[7,6,3],[4,3,6,7]],[[6,3,5,9],[9,8,5],[6,5,8,9]]).
p([[7,7,2,1],[4,1,7,2],[3,7,4],[0,0,1]],[[9,9,4,3],[6,3,9,4],[5,9,6],[2,2,3]]).
p([[3,1,1],[2,6,7,5]],[[5,3,3],[4,8,9,7]]).
q([[7,2,5],[5,2,0],[6,6,7,0],[5,7,4]],[[7,2,5],[7,4,2],[8,8,9,2],[7,9,6]]).
q([[7,6,1,1],[2,0,2],[4,2,3,2]],[[7,6,1,1],[4,2,4],[6,4,5,4]]).
q([[7,7,1],[7,0,3],[4,3,4],[1,7,1,2]],[[7,7,1],[9,2,5],[6,5,6],[3,9,3,4]]).
q([[5,2,6,5],[3,3,1],[5,2,6],[4,6,7]],[[5,2,6,5],[3,3,1],[7,4,8],[6,8,9]]).
q([[5,0,5],[7,2,1,3],[5,3,5]],[[7,2,7],[7,2,1,3],[7,5,7]]).
