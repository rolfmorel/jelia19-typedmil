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
my_even2(A):-0 is A mod 2.
my_msort3(A,B):-msort(A,B).
my_set4(A):-list_to_set(A,A).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_odd8(A):-1 is A mod 2.
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_tail11([_|TL],TL).
my_len12(A,B):-length(A,B).

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

my_double14(N,M):-M is 2*N,M =< 10.
prim(my_succ1/2).
prim(my_even2/1).
prim(my_msort3/2).
prim(my_set4/1).
prim(my_reverse5/2).
prim(my_sumlist6/2).
prim(my_tolower7/2).
prim(my_odd8/1).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_tail11/2).
prim(my_len12/2).
prim(my_double14/2).
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
p([[0,7,0],[6,0,7]],[[2,9,2],[8,2,9]]).
p([[7,5,2,5],[6,6,3,1],[7,5,2,0]],[[9,7,4,7],[8,8,5,3],[9,7,4,2]]).
p([[7,5,2],[2,7,3]],[[9,7,4],[4,9,5]]).
p([[3,2,0],[4,6,4,7],[7,5,4,7]],[[5,4,2],[6,8,6,9],[9,7,6,9]]).
p([[4,7,4],[5,7,6],[5,4,3],[2,6,6]],[[6,9,6],[7,9,8],[7,6,5],[4,8,8]]).
q([[6,3,4],[7,2,6],[5,0,4]],[[8,5,6],[7,2,6],[7,2,6]]).
q([[2,1,2],[1,7,5,6],[1,3,1],[1,2,3,7]],[[2,1,2],[1,7,5,6],[3,5,3],[3,4,5,9]]).
q([[0,2,3],[2,2,7,7],[6,6,6,3],[7,3,7,7]],[[2,4,5],[2,2,7,7],[8,8,8,5],[9,5,9,9]]).
q([[1,3,7,4],[7,3,2]],[[3,5,9,6],[7,3,2]]).
q([[7,6,0],[7,6,7],[1,0,3],[1,1,2]],[[9,8,2],[9,8,9],[3,2,5],[1,1,2]]).
