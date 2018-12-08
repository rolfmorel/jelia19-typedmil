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
my_max_list0(A,B):-max_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_len3(A,B):-length(A,B).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_min_list10(A,B):-min_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_succ13(A,B):-succ(A,B).
my_len14(A,B):-length(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_reverse16(A,B):-reverse(A,B).
my_succ17(A,B):-succ(A,B).
my_pred18(A,B):-succ(B,A).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_head2/2).
prim(my_len3/2).
prim(my_max_list4/2).
prim(my_succ5/2).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_len8/2).
prim(my_reverse9/2).
prim(my_min_list10/2).
prim(my_sumlist11/2).
prim(my_len12/2).
prim(my_succ13/2).
prim(my_len14/2).
prim(my_sumlist15/2).
prim(my_reverse16/2).
prim(my_succ17/2).
prim(my_pred18/2).
prim(my_pred19/2).
prim(my_pred20/2).
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
p([['q','x','s'],['f','x','q']],[['q','x'],['f','x']]).
p([['e','w','t'],['p','x','h','y'],['f','q','n','x'],['e','t','o']],[['e','w'],['p','x','h'],['f','q','n'],['e','t']]).
