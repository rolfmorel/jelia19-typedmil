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
my_len2(A,B):-length(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_element4(A,B):-member(B,A).

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

my_list_to_set6(A,B):-list_to_set(A,B).
my_reverse7(A,B):-reverse(A,B).
my_head8([H|_],H).
my_msort9(A,B):-msort(A,B).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_set11(A):-list_to_set(A,A).
my_odd12(A):-1 is A mod 2.
my_pred13(A,B):-succ(B,A),A > 0.
my_even14(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_len2/2).
prim(my_sumlist3/2).
prim(my_element4/2).
prim(my_list_to_set6/2).
prim(my_reverse7/2).
prim(my_head8/2).
prim(my_msort9/2).
prim(my_tolower10/2).
prim(my_set11/1).
prim(my_odd12/1).
prim(my_pred13/2).
prim(my_even14/1).
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
p([[5,7,1],[5,5,5,6]],[[7,9,3],[7,7,7,8]]).
p([[0,3,3],[1,0,7,3],[1,2,4],[5,6,3]],[[2,5,5],[3,2,9,5],[3,4,6],[7,8,5]]).
p([[2,4,3],[3,0,5,0],[0,0,7]],[[4,6,5],[5,2,7,2],[2,2,9]]).
p([[6,1,1],[5,5,2],[2,1,0,6],[1,2,7,2]],[[8,3,3],[7,7,4],[4,3,2,8],[3,4,9,4]]).
p([[5,1,6,4],[0,3,5]],[[7,3,8,6],[2,5,7]]).
q([[7,0,1],[4,3,7,1]],[[7,0,1],[6,5,9,3]]).
q([[7,3,0],[3,0,6],[0,7,2]],[[7,3,0],[5,2,8],[2,9,4]]).
q([[3,3,5,5],[5,4,1,6]],[[5,5,7,7],[5,4,1,6]]).
q([[6,1,7,4],[5,2,5],[4,2,1,2],[4,5,3]],[[6,1,7,4],[7,4,7],[4,2,1,2],[6,7,5]]).
q([[5,5,4,3],[3,4,4,7],[6,3,0,2],[2,3,4,1]],[[7,7,6,5],[5,6,6,9],[8,5,2,4],[2,3,4,1]]).
