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
my_pred2(A,B):-succ(B,A).
my_last3(A,B):-last(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_tail7([_|TL],TL).
my_min_list8(A,B):-min_list(A,B).
my_len9(A,B):-length(A,B).
my_tail10([_|TL],TL).
my_last11(A,B):-last(A,B).
my_tail12([_|TL],TL).
my_succ13(A,B):-succ(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_tail15([_|TL],TL).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_pred2/2).
prim(my_last3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_tail7/2).
prim(my_min_list8/2).
prim(my_len9/2).
prim(my_tail10/2).
prim(my_last11/2).
prim(my_tail12/2).
prim(my_succ13/2).
prim(my_sumlist14/2).
prim(my_tail15/2).
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
p([['a','s','i','o'],['f','j','l']],[['a','s','i'],['f','j']]).
p([['g','p','o','h'],['o','p','i','r'],['x','s','y']],[['g','p','o'],['o','p','i'],['x','s']]).
