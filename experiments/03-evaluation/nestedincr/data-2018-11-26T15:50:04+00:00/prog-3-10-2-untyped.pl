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
my_element2(A,B):-member(B,A).

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

my_double4(N,M):-M is 2*N,M =< 10.
my_set5(A):-list_to_set(A,A).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten9(A,B):-flatten(A,B).
my_msort10(A,B):-msort(A,B).
my_last11(A,B):-last(A,B).
prim(my_succ1/2).
prim(my_element2/2).
prim(my_double4/2).
prim(my_set5/1).
prim(my_len6/2).
prim(my_pred7/2).
prim(my_tolower8/2).
prim(my_flatten9/2).
prim(my_msort10/2).
prim(my_last11/2).
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
p([[7,5,5],[6,6,5,0],[1,5,7]],[[9,7,7],[8,8,7,2],[3,7,9]]).
p([[1,6,1,3],[6,7,5],[2,7,3],[6,5,6]],[[3,8,3,5],[8,9,7],[4,9,5],[8,7,8]]).
p([[1,1,7],[6,6,3],[1,2,1,3]],[[3,3,9],[8,8,5],[3,4,3,5]]).
p([[2,4,6],[5,5,1,7]],[[4,6,8],[7,7,3,9]]).
p([[7,3,5,7],[3,0,3,1],[6,0,1],[5,5,4,2]],[[9,5,7,9],[5,2,5,3],[8,2,3],[7,7,6,4]]).
q([[7,4,6],[2,1,0,5]],[[9,6,8],[2,1,0,5]]).
q([[5,5,5],[0,4,7],[6,1,4],[7,6,3,5]],[[5,5,5],[2,6,9],[6,1,4],[9,8,5,7]]).
q([[0,1,6,7],[4,4,4],[0,2,0,5]],[[2,3,8,9],[4,4,4],[2,4,2,7]]).
q([[7,0,4],[0,6,3,7],[0,7,0],[3,3,0]],[[9,2,6],[2,8,5,9],[0,7,0],[5,5,2]]).
q([[3,4,5,7],[7,6,2,0]],[[5,6,7,9],[7,6,2,0]]).
