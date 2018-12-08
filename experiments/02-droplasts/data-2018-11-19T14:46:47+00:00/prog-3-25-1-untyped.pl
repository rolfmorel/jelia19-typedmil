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
my_sumlist1(A,B):-sumlist(A,B).
my_tail2([_|TL],TL).
my_len3(A,B):-length(A,B).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_last10(A,B):-last(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_pred14(A,B):-succ(B,A).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_last19(A,B):-last(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_sumlist0/2).
prim(my_sumlist1/2).
prim(my_tail2/2).
prim(my_len3/2).
prim(my_max_list4/2).
prim(my_succ5/2).
prim(my_pred6/2).
prim(my_reverse7/2).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_last10/2).
prim(my_max_list11/2).
prim(my_tail12/2).
prim(my_sumlist13/2).
prim(my_pred14/2).
prim(my_sumlist15/2).
prim(my_pred16/2).
prim(my_head17/2).
prim(my_reverse18/2).
prim(my_last19/2).
prim(my_sumlist20/2).
prim(my_sumlist21/2).
prim(my_max_list22/2).
prim(my_last23/2).
prim(my_sumlist24/2).
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
p([['o','n','j','x'],['g','q','i','n'],['k','y','h'],['k','v','r']],[['o','n','j'],['g','q','i'],['k','y'],['k','v']]).
p([['x','s','b'],['k','g','r','t']],[['x','s'],['k','g','r']]).
