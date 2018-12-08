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
my_len0(A,B):-length(A,B).
my_pred1(A,B):-succ(B,A).
my_succ2(A,B):-succ(A,B).
my_len3(A,B):-length(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_reverse6(A,B):-reverse(A,B).
my_reverse7(A,B):-reverse(A,B).
my_tail8([_|TL],TL).
my_max_list9(A,B):-max_list(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_max_list13(A,B):-max_list(A,B).
prim(my_len0/2).
prim(my_pred1/2).
prim(my_succ2/2).
prim(my_len3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_reverse6/2).
prim(my_reverse7/2).
prim(my_tail8/2).
prim(my_max_list9/2).
prim(my_len10/2).
prim(my_sumlist11/2).
prim(my_sumlist12/2).
prim(my_max_list13/2).
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
p([['w','p','p','r'],['u','p','n','c']],[['w','p','p'],['u','p','n']]).
p([['n','v','y'],['d','m','q','g'],['c','w','w','p'],['e','e','e','h']],[['n','v'],['d','m','q'],['c','w','w'],['e','e','e']]).
