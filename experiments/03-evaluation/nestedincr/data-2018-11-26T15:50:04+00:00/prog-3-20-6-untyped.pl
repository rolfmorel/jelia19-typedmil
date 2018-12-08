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
my_max_list3(A,B):-max_list(A,B).
my_flatten4(A,B):-flatten(A,B).
my_tail5([_|TL],TL).
my_min_list6(A,B):-min_list(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_element10(A,B):-member(B,A).
my_even11(A):-0 is A mod 2.
my_pred12(A,B):-succ(B,A),A > 0.
my_list_to_set13(A,B):-list_to_set(A,B).
my_tolower14(A,B):-downcase_atom(A,B),char_code(A,_).
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_set18(A):-list_to_set(A,A).

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

my_msort20(A,B):-msort(A,B).
my_odd21(A):-1 is A mod 2.
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_max_list3/2).
prim(my_flatten4/2).
prim(my_tail5/2).
prim(my_min_list6/2).
prim(my_double7/2).
prim(my_len8/2).
prim(my_reverse9/2).
prim(my_element10/2).
prim(my_even11/1).
prim(my_pred12/2).
prim(my_list_to_set13/2).
prim(my_tolower14/2).
prim(my_uppercase15/1).
prim(my_sumlist16/2).
prim(my_head17/2).
prim(my_set18/1).
prim(my_msort20/2).
prim(my_odd21/1).
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
p([[0,3,5],[3,0,5,5],[0,0,6,1]],[[2,5,7],[5,2,7,7],[2,2,8,3]]).
p([[7,2,7,6],[6,4,7,7],[6,3,1,1],[6,6,2]],[[9,4,9,8],[8,6,9,9],[8,5,3,3],[8,8,4]]).
p([[0,1,3],[7,2,3]],[[2,3,5],[9,4,5]]).
p([[0,4,2],[0,3,1,4],[0,1,5],[3,3,0,7]],[[2,6,4],[2,5,3,6],[2,3,7],[5,5,2,9]]).
p([[6,5,0],[5,7,7,1]],[[8,7,2],[7,9,9,3]]).
q([[5,7,0,7],[1,6,7,1],[3,3,7,3]],[[5,7,0,7],[3,8,9,3],[5,5,9,5]]).
q([[0,7,1,0],[0,0,6]],[[0,7,1,0],[2,2,8]]).
q([[2,0,7,2],[6,2,5,4],[6,4,4,3]],[[2,0,7,2],[8,4,7,6],[8,6,6,5]]).
q([[7,0,2],[5,3,4],[2,3,7,6]],[[7,0,2],[7,5,6],[4,5,9,8]]).
q([[6,7,0,2],[4,0,5,2],[3,5,7],[2,0,7,0]],[[8,9,2,4],[6,2,7,4],[3,5,7],[2,0,7,0]]).
