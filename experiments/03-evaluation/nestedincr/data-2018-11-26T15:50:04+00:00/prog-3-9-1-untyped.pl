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

my_sumlist4(A,B):-sumlist(A,B).
my_max_list5(A,B):-max_list(A,B).
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list7(A,B):-min_list(A,B).
my_head8([H|_],H).
my_tail9([_|TL],TL).
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_sumlist4/2).
prim(my_max_list5/2).
prim(my_tolower6/2).
prim(my_min_list7/2).
prim(my_head8/2).
prim(my_tail9/2).
prim(my_uppercase10/1).
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
p([[0,2,0],[3,7,3],[1,4,3]],[[2,4,2],[5,9,5],[3,6,5]]).
p([[5,0,5,1],[5,4,7]],[[7,2,7,3],[7,6,9]]).
p([[4,5,7],[5,7,2],[2,0,0],[7,1,4,2]],[[6,7,9],[7,9,4],[4,2,2],[9,3,6,4]]).
p([[1,4,4],[1,1,1],[7,2,2]],[[3,6,6],[3,3,3],[9,4,4]]).
p([[2,5,7],[6,7,2,2],[3,5,3]],[[4,7,9],[8,9,4,4],[5,7,5]]).
q([[2,7,3,4],[3,6,1],[0,7,3]],[[4,9,5,6],[3,6,1],[2,9,5]]).
q([[7,3,3,3],[3,7,6]],[[9,5,5,5],[3,7,6]]).
q([[6,0,4,3],[6,6,4]],[[8,2,6,5],[6,6,4]]).
q([[6,7,3],[4,4,0],[7,7,0,1]],[[8,9,5],[6,6,2],[7,7,0,1]]).
q([[2,1,6],[4,3,7,4],[2,4,5]],[[4,3,8],[4,3,7,4],[4,6,7]]).
