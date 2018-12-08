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
my_min_list2(A,B):-min_list(A,B).
my_len3(A,B):-length(A,B).
my_set4(A):-list_to_set(A,A).
my_sumlist5(A,B):-sumlist(A,B).
my_odd6(A):-1 is A mod 2.
my_tail7([_|TL],TL).
my_element8(A,B):-member(B,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_last10(A,B):-last(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_head13([H|_],H).
my_flatten14(A,B):-flatten(A,B).
my_msort15(A,B):-msort(A,B).
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_len3/2).
prim(my_set4/1).
prim(my_sumlist5/2).
prim(my_odd6/1).
prim(my_tail7/2).
prim(my_element8/2).
prim(my_pred9/2).
prim(my_last10/2).
prim(my_toupper11/2).
prim(my_tolower12/2).
prim(my_head13/2).
prim(my_flatten14/2).
prim(my_msort15/2).
prim(my_lowercase16/1).
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
p([[0,6,6],[4,0,5],[5,2,3],[1,5,4,1]],[[2,8,8],[6,2,7],[7,4,5],[3,7,6,3]]).
p([[0,7,2,5],[2,1,0,6],[3,1,7],[3,7,1,7]],[[2,9,4,7],[4,3,2,8],[5,3,9],[5,9,3,9]]).
p([[7,2,3,3],[6,1,5],[6,1,2,7],[6,5,6]],[[9,4,5,5],[8,3,7],[8,3,4,9],[8,7,8]]).
p([[7,6,2],[6,7,1],[3,2,5]],[[9,8,4],[8,9,3],[5,4,7]]).
p([[1,0,7,0],[5,7,6,3],[1,5,3,3]],[[3,2,9,2],[7,9,8,5],[3,7,5,5]]).
q([[1,1,2],[4,4,6],[4,0,2,2],[0,0,3,2]],[[3,3,4],[4,4,6],[6,2,4,4],[2,2,5,4]]).
q([[2,7,7],[5,5,4,1],[5,1,4],[7,6,0]],[[4,9,9],[5,5,4,1],[7,3,6],[9,8,2]]).
q([[5,2,4,4],[6,7,3]],[[5,2,4,4],[8,9,5]]).
q([[3,6,7],[0,5,1,4],[4,6,7,3],[3,5,3,2]],[[5,8,9],[2,7,3,6],[4,6,7,3],[5,7,5,4]]).
q([[4,7,1],[3,5,7,0],[7,5,1],[5,1,4,6]],[[6,9,3],[3,5,7,0],[9,7,3],[7,3,6,8]]).
