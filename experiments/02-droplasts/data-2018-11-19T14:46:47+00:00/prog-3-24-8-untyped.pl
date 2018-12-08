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
my_pred0(A,B):-succ(B,A).
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_pred12(A,B):-succ(B,A).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_min_list23(A,B):-min_list(A,B).
prim(my_pred0/2).
prim(my_reverse1/2).
prim(my_reverse2/2).
prim(my_sumlist3/2).
prim(my_max_list4/2).
prim(my_sumlist5/2).
prim(my_head6/2).
prim(my_reverse7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_succ10/2).
prim(my_last11/2).
prim(my_pred12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_max_list15/2).
prim(my_reverse16/2).
prim(my_tail17/2).
prim(my_len18/2).
prim(my_head19/2).
prim(my_min_list20/2).
prim(my_sumlist21/2).
prim(my_sumlist22/2).
prim(my_min_list23/2).
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
p([['i','q','n','m'],['w','j','m','n'],['o','t','h','v'],['a','l','c']],[['i','q','n'],['w','j','m'],['o','t','h'],['a','l']]).
p([['y','u','g','l'],['a','y','i'],['w','k','j','x'],['j','d','x','e']],[['y','u','g'],['a','y'],['w','k','j'],['j','d','x']]).
