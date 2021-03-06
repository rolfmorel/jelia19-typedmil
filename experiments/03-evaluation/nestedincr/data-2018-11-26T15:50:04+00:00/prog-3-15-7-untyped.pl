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
my_odd3(A):-1 is A mod 2.
my_pred4(A,B):-succ(B,A),A > 0.
my_last5(A,B):-last(A,B).
my_msort6(A,B):-msort(A,B).
my_element7(A,B):-member(B,A).
my_double8(N,M):-M is 2*N,M =< 10.
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_len11(A,B):-length(A,B).
my_flatten12(A,B):-flatten(A,B).
my_tail13([_|TL],TL).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_min_list15(A,B):-min_list(A,B).
my_set16(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_odd3/1).
prim(my_pred4/2).
prim(my_last5/2).
prim(my_msort6/2).
prim(my_element7/2).
prim(my_double8/2).
prim(my_lowercase9/1).
prim(my_toupper10/2).
prim(my_len11/2).
prim(my_flatten12/2).
prim(my_tail13/2).
prim(my_uppercase14/1).
prim(my_min_list15/2).
prim(my_set16/1).
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
p([[4,7,2],[0,2,7,2]],[[6,9,4],[2,4,9,4]]).
p([[6,0,3],[5,2,2],[2,1,0]],[[8,2,5],[7,4,4],[4,3,2]]).
p([[3,3,0],[2,7,4,1],[3,4,0]],[[5,5,2],[4,9,6,3],[5,6,2]]).
p([[2,6,4],[3,0,6],[3,1,7]],[[4,8,6],[5,2,8],[5,3,9]]).
p([[3,2,1,0],[3,5,4,6],[0,0,1],[1,2,1,2]],[[5,4,3,2],[5,7,6,8],[2,2,3],[3,4,3,4]]).
q([[4,1,0],[2,0,6,4],[7,2,0,7]],[[4,1,0],[4,2,8,6],[9,4,2,9]]).
q([[3,7,1,5],[0,4,6,7],[5,5,5,0]],[[5,9,3,7],[0,4,6,7],[7,7,7,2]]).
q([[3,4,3],[0,3,6],[6,3,3],[1,7,0]],[[5,6,5],[2,5,8],[6,3,3],[3,9,2]]).
q([[5,2,4,5],[7,1,4,7],[4,1,0,0]],[[7,4,6,7],[7,1,4,7],[6,3,2,2]]).
q([[4,3,3],[7,4,6,5],[4,0,1],[5,3,7,3]],[[4,3,3],[9,6,8,7],[6,2,3],[7,5,9,5]]).
