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
my_min_list0(A,B):-min_list(A,B).
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_max_list7(A,B):-max_list(A,B).
my_head8([H|_],H).
my_reverse9(A,B):-reverse(A,B).
my_len10(A,B):-length(A,B).
my_min_list11(A,B):-min_list(A,B).
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
my_max_list14(A,B):-max_list(A,B).
my_last15(A,B):-last(A,B).
prim(my_min_list0/2).
prim(my_pred1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_max_list7/2).
prim(my_head8/2).
prim(my_reverse9/2).
prim(my_len10/2).
prim(my_min_list11/2).
prim(my_last12/2).
prim(my_min_list13/2).
prim(my_max_list14/2).
prim(my_last15/2).
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
p([['c','q','v','s'],['u','v','u','w']],[['c','q','v'],['u','v','u']]).
p([['w','m','r','y'],['l','c','a']],[['w','m','r'],['l','c']]).
