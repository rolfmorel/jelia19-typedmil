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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_odd4(A):-1 is A mod 2.
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_double7(N,M):-M is 2*N,M =< 10.
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_max_list9(A,B):-max_list(A,B).
my_flatten10(A,B):-flatten(A,B).
my_last11(A,B):-last(A,B).
my_tail12([_|TL],TL).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_even14(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_odd4/1).
prim(my_len5/2).
prim(my_set6/1).
prim(my_double7/2).
prim(my_uppercase8/1).
prim(my_max_list9/2).
prim(my_flatten10/2).
prim(my_last11/2).
prim(my_tail12/2).
prim(my_lowercase13/1).
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
p([[7,2,5,1],[5,3,0,1]],[[9,4,7,3],[7,5,2,3]]).
p([[1,6,4,0],[4,3,2]],[[3,8,6,2],[6,5,4]]).
p([[5,0,7],[0,5,3],[2,2,1],[2,5,0]],[[7,2,9],[2,7,5],[4,4,3],[4,7,2]]).
p([[6,2,6,2],[0,6,5],[2,6,3,2]],[[8,4,8,4],[2,8,7],[4,8,5,4]]).
p([[7,1,2],[0,7,1,2],[0,4,3],[7,7,1]],[[9,3,4],[2,9,3,4],[2,6,5],[9,9,3]]).
q([[7,3,1,4],[3,4,3]],[[7,3,1,4],[5,6,5]]).
q([[4,7,4],[2,5,2],[6,1,5,5]],[[4,7,4],[4,7,4],[8,3,7,7]]).
q([[6,7,4],[0,5,4,6],[0,2,4,1],[3,4,5,4]],[[8,9,6],[0,5,4,6],[0,2,4,1],[5,6,7,6]]).
q([[7,5,3],[6,5,7],[0,4,7,2],[7,0,0]],[[9,7,5],[6,5,7],[0,4,7,2],[9,2,2]]).
q([[0,5,0],[3,5,5]],[[2,7,2],[3,5,5]]).
