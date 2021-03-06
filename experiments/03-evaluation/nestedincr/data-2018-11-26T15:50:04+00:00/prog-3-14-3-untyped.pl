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
my_tail2([_|TL],TL).
my_double3(N,M):-M is 2*N,M =< 10.
my_even4(A):-0 is A mod 2.
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).

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

my_len9(A,B):-length(A,B).
my_reverse10(A,B):-reverse(A,B).
my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).
my_element12(A,B):-member(B,A).
my_toupper13(A,B):-upcase_atom(A,B),char_code(A,_).
my_odd14(A):-1 is A mod 2.
my_max_list15(A,B):-max_list(A,B).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_double3/2).
prim(my_even4/1).
prim(my_tolower5/2).
prim(my_set6/1).
prim(my_list_to_set7/2).
prim(my_len9/2).
prim(my_reverse10/2).
prim(my_lowercase11/1).
prim(my_element12/2).
prim(my_toupper13/2).
prim(my_odd14/1).
prim(my_max_list15/2).
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
p([[3,0,2,7],[1,6,3],[4,7,6]],[[5,2,4,9],[3,8,5],[6,9,8]]).
p([[4,4,1],[2,7,1],[7,1,4]],[[6,6,3],[4,9,3],[9,3,6]]).
p([[3,3,7],[6,7,1],[5,6,0],[4,6,5]],[[5,5,9],[8,9,3],[7,8,2],[6,8,7]]).
p([[5,3,5,7],[0,3,6]],[[7,5,7,9],[2,5,8]]).
p([[4,2,1,2],[6,0,0]],[[6,4,3,4],[8,2,2]]).
q([[4,1,1,0],[2,2,5,0],[3,3,5,7],[3,4,4]],[[6,3,3,2],[4,4,7,2],[5,5,7,9],[3,4,4]]).
q([[6,4,6,0],[7,5,1],[3,3,1],[6,2,0,5]],[[6,4,6,0],[9,7,3],[5,5,3],[8,4,2,7]]).
q([[1,1,7,7],[3,6,6,3],[0,6,5,5],[5,0,1]],[[3,3,9,9],[3,6,6,3],[2,8,7,7],[7,2,3]]).
q([[2,7,2,4],[3,4,1,1],[7,7,0,3]],[[4,9,4,6],[3,4,1,1],[9,9,2,5]]).
q([[5,1,1],[3,7,2,6]],[[5,1,1],[5,9,4,8]]).
