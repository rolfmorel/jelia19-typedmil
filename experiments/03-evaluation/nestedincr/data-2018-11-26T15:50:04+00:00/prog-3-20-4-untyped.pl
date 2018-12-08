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
my_len3(A,B):-length(A,B).
my_even4(A):-0 is A mod 2.
my_last5(A,B):-last(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_flatten7(A,B):-flatten(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_set12(A):-list_to_set(A,A).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).

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

my_min_list15(A,B):-min_list(A,B).
my_msort16(A,B):-msort(A,B).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
my_head18([H|_],H).
my_element19(A,B):-member(B,A).
my_tail20([_|TL],TL).
my_reverse21(A,B):-reverse(A,B).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_len3/2).
prim(my_even4/1).
prim(my_last5/2).
prim(my_list_to_set6/2).
prim(my_flatten7/2).
prim(my_double8/2).
prim(my_max_list9/2).
prim(my_pred10/2).
prim(my_odd11/1).
prim(my_set12/1).
prim(my_lowercase13/1).
prim(my_min_list15/2).
prim(my_msort16/2).
prim(my_tolower17/2).
prim(my_head18/2).
prim(my_element19/2).
prim(my_tail20/2).
prim(my_reverse21/2).
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
p([[6,1,1],[5,2,5]],[[8,3,3],[7,4,7]]).
p([[6,0,0],[3,7,7,6],[3,2,1]],[[8,2,2],[5,9,9,8],[5,4,3]]).
p([[1,5,7],[2,2,0,7],[1,0,3],[6,7,2]],[[3,7,9],[4,4,2,9],[3,2,5],[8,9,4]]).
p([[3,6,0,7],[5,6,2,0],[1,7,6,2]],[[5,8,2,9],[7,8,4,2],[3,9,8,4]]).
p([[1,5,0,6],[4,4,0],[5,0,2],[0,6,7]],[[3,7,2,8],[6,6,2],[7,2,4],[2,8,9]]).
q([[6,4,0],[7,4,0],[1,5,1],[2,4,1,7]],[[8,6,2],[7,4,0],[3,7,3],[2,4,1,7]]).
q([[6,0,7],[4,4,4,5],[5,2,1,3]],[[6,0,7],[6,6,6,7],[7,4,3,5]]).
q([[7,7,2],[2,4,2],[2,1,3,4]],[[9,9,4],[4,6,4],[2,1,3,4]]).
q([[6,7,6],[3,5,4]],[[8,9,8],[3,5,4]]).
q([[6,6,3],[5,6,0]],[[6,6,3],[7,8,2]]).
