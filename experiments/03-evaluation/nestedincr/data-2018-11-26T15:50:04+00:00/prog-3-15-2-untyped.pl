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
my_flatten2(A,B):-flatten(A,B).
my_element3(A,B):-member(B,A).
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist6(A,B):-sumlist(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_even10(A):-0 is A mod 2.
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_msort12(A,B):-msort(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_double14(N,M):-M is 2*N,M =< 10.
my_tail15([_|TL],TL).
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_flatten2/2).
prim(my_element3/2).
prim(my_toupper4/2).
prim(my_lowercase5/1).
prim(my_sumlist6/2).
prim(my_list_to_set7/2).
prim(my_last8/2).
prim(my_reverse9/2).
prim(my_even10/1).
prim(my_uppercase11/1).
prim(my_msort12/2).
prim(my_pred13/2).
prim(my_double14/2).
prim(my_tail15/2).
prim(my_tolower16/2).
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
p([[3,6,0,3],[3,0,6,5],[6,4,6,3],[7,7,6]],[[5,8,2,5],[5,2,8,7],[8,6,8,5],[9,9,8]]).
p([[4,3,6,3],[4,1,3,1]],[[6,5,8,5],[6,3,5,3]]).
p([[0,6,6,2],[1,1,3]],[[2,8,8,4],[3,3,5]]).
p([[7,7,3,1],[3,6,1,4],[7,1,5],[1,4,2,2]],[[9,9,5,3],[5,8,3,6],[9,3,7],[3,6,4,4]]).
p([[3,3,1],[7,2,1],[4,7,0,6],[5,6,0]],[[5,5,3],[9,4,3],[6,9,2,8],[7,8,2]]).
q([[5,0,2],[6,5,4,5],[2,4,4]],[[5,0,2],[8,7,6,7],[4,6,6]]).
q([[1,0,3,4],[3,1,2],[2,2,3,7],[6,5,1]],[[3,2,5,6],[3,1,2],[4,4,5,9],[8,7,3]]).
q([[4,7,4],[2,2,1],[1,5,3,3]],[[6,9,6],[2,2,1],[3,7,5,5]]).
q([[7,7,0,4],[5,3,6,5],[4,5,4],[6,1,2]],[[7,7,0,4],[7,5,8,7],[4,5,4],[8,3,4]]).
q([[5,4,5],[3,4,2],[1,3,7]],[[7,6,7],[5,6,4],[1,3,7]]).
