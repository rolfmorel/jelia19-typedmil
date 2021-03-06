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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_len3(A,B):-length(A,B).
my_head4([H|_],H).

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

my_pred6(A,B):-succ(B,A),A > 0.
my_tail7([_|TL],TL).
my_sumlist8(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_len3/2).
prim(my_head4/2).
prim(my_pred6/2).
prim(my_tail7/2).
prim(my_sumlist8/2).
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
p([[1,7,1,7],[4,3,2],[4,1,1],[6,6,0,2]],[[3,9,3,9],[6,5,4],[6,3,3],[8,8,2,4]]).
p([[0,6,1,5],[2,6,4],[6,5,7,1],[1,7,2,0]],[[2,8,3,7],[4,8,6],[8,7,9,3],[3,9,4,2]]).
p([[1,5,7,0],[2,7,5],[2,5,6,4],[3,2,4]],[[3,7,9,2],[4,9,7],[4,7,8,6],[5,4,6]]).
p([[6,1,2],[6,6,4,4],[6,0,5]],[[8,3,4],[8,8,6,6],[8,2,7]]).
p([[0,0,5],[4,0,0],[7,2,1,7],[1,2,7]],[[2,2,7],[6,2,2],[9,4,3,9],[3,4,9]]).
q([[6,3,5],[5,4,6]],[[6,3,5],[7,6,8]]).
q([[4,5,5],[0,6,3,7],[2,1,2,3],[3,6,5,0]],[[6,7,7],[2,8,5,9],[4,3,4,5],[3,6,5,0]]).
q([[2,7,7,2],[0,6,5,1],[6,5,3],[0,4,7]],[[4,9,9,4],[0,6,5,1],[8,7,5],[2,6,9]]).
q([[2,4,5,2],[6,6,5,6],[5,1,4]],[[4,6,7,4],[8,8,7,8],[5,1,4]]).
q([[6,5,0],[0,7,3],[6,1,5],[2,0,1]],[[6,5,0],[0,7,3],[8,3,7],[4,2,3]]).
