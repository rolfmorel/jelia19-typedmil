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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_pred3(A,B):-succ(B,A),A > 0.
my_reverse4(A,B):-reverse(A,B).
my_msort5(A,B):-msort(A,B).
my_tail6([_|TL],TL).
my_list_to_set7(A,B):-list_to_set(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_odd9(A):-1 is A mod 2.

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

my_even11(A):-0 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_element13(A,B):-member(B,A).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_uppercase2/1).
prim(my_pred3/2).
prim(my_reverse4/2).
prim(my_msort5/2).
prim(my_tail6/2).
prim(my_list_to_set7/2).
prim(my_lowercase8/1).
prim(my_odd9/1).
prim(my_even11/1).
prim(my_tolower12/2).
prim(my_element13/2).
prim(my_toupper14/2).
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
p([[4,2,3,4],[2,3,2]],[[6,4,5,6],[4,5,4]]).
p([[7,5,1,7],[0,5,3,2]],[[9,7,3,9],[2,7,5,4]]).
p([[4,1,1,4],[5,4,0,3],[1,0,4]],[[6,3,3,6],[7,6,2,5],[3,2,6]]).
p([[7,4,2,6],[7,7,7,5]],[[9,6,4,8],[9,9,9,7]]).
p([[6,2,4],[5,6,4]],[[8,4,6],[7,8,6]]).
q([[2,5,6],[5,6,1],[1,7,4],[3,1,4,5]],[[4,7,8],[5,6,1],[3,9,6],[5,3,6,7]]).
q([[3,5,4],[3,7,7],[1,3,6,1],[7,4,6]],[[5,7,6],[5,9,9],[1,3,6,1],[7,4,6]]).
q([[1,4,5],[7,7,3],[1,4,1],[6,6,3,6]],[[1,4,5],[7,7,3],[3,6,3],[8,8,5,8]]).
q([[2,1,0,4],[5,5,3,0]],[[4,3,2,6],[5,5,3,0]]).
q([[5,7,1,5],[5,2,2],[1,2,6]],[[5,7,1,5],[7,4,4],[3,4,8]]).
