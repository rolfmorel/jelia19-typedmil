:- use_module('metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail/2).
prim(reverse/2).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
my_sumlist0(A,B):-sumlist(A,B).
my_pred1(A,B):-succ(B,A).
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_last5(A,B):-last(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_succ9(A,B):-succ(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_len11(A,B):-length(A,B).
my_reverse12(A,B):-reverse(A,B).
my_head13([H|_],H).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_last16(A,B):-last(A,B).
prim(my_sumlist0/2).
prim(my_pred1/2).
prim(my_head2/2).
prim(my_min_list3/2).
prim(my_max_list4/2).
prim(my_last5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_min_list8/2).
prim(my_succ9/2).
prim(my_sumlist10/2).
prim(my_len11/2).
prim(my_reverse12/2).
prim(my_head13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
prim(my_last16/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learn(Pos,[],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['w','j','i'],['f','e','e']],[['w','j'],['f','e']]).
p([['w','k','i'],['i','g','v','j']],[['w','k'],['i','g','v']]).
