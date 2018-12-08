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
my_succ0(A,B):-succ(A,B).
my_min_list1(A,B):-min_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_min_list4(A,B):-min_list(A,B).
my_last5(A,B):-last(A,B).
my_pred6(A,B):-succ(B,A).
my_last7(A,B):-last(A,B).
my_tail8([_|TL],TL).
my_max_list9(A,B):-max_list(A,B).
my_len10(A,B):-length(A,B).
my_pred11(A,B):-succ(B,A).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_min_list14(A,B):-min_list(A,B).
prim(my_succ0/2).
prim(my_min_list1/2).
prim(my_reverse2/2).
prim(my_reverse3/2).
prim(my_min_list4/2).
prim(my_last5/2).
prim(my_pred6/2).
prim(my_last7/2).
prim(my_tail8/2).
prim(my_max_list9/2).
prim(my_len10/2).
prim(my_pred11/2).
prim(my_head12/2).
prim(my_min_list13/2).
prim(my_min_list14/2).
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
p([['i','p','w'],['p','t','p'],['g','g','r'],['q','l','u']],[['i','p'],['p','t'],['g','g'],['q','l']]).
p([['u','j','r','l'],['h','d','m','o'],['j','i','m','q'],['p','p','t','l']],[['u','j','r'],['h','d','m'],['j','i','m'],['p','p','t']]).
