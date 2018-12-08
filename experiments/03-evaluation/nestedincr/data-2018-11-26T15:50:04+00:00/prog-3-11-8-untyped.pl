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
my_double2(N,M):-M is 2*N,M =< 10.
my_head3([H|_],H).
my_pred4(A,B):-succ(B,A),A > 0.
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_odd7(A):-1 is A mod 2.
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_msort9(A,B):-msort(A,B).
my_reverse10(A,B):-reverse(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_element12(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_double2/2).
prim(my_head3/2).
prim(my_pred4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_odd7/1).
prim(my_toupper8/2).
prim(my_msort9/2).
prim(my_reverse10/2).
prim(my_uppercase11/1).
prim(my_element12/2).
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
p([[5,3,1],[0,1,4,1],[2,2,5]],[[7,5,3],[2,3,6,3],[4,4,7]]).
p([[7,6,7],[2,7,2],[0,3,7],[2,0,7]],[[9,8,9],[4,9,4],[2,5,9],[4,2,9]]).
p([[2,2,6,6],[5,6,5]],[[4,4,8,8],[7,8,7]]).
p([[5,6,1],[6,2,0,2],[6,2,6,5],[4,0,5,5]],[[7,8,3],[8,4,2,4],[8,4,8,7],[6,2,7,7]]).
p([[0,2,5,3],[7,2,7],[6,4,7,4]],[[2,4,7,5],[9,4,9],[8,6,9,6]]).
q([[6,3,3,3],[6,0,3,0],[0,1,2],[3,4,2]],[[6,3,3,3],[6,0,3,0],[2,3,4],[5,6,4]]).
q([[6,6,4,2],[6,4,3,3],[2,5,1]],[[8,8,6,4],[6,4,3,3],[4,7,3]]).
q([[0,0,4],[2,7,7,4]],[[0,0,4],[4,9,9,6]]).
q([[1,5,7,1],[3,2,4,0]],[[1,5,7,1],[5,4,6,2]]).
q([[0,2,5],[7,2,2],[1,0,6]],[[2,4,7],[9,4,4],[1,0,6]]).
