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
my_head1([H|_],H).
my_len2(A,B):-length(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
prim(my_min_list0/2).
prim(my_head1/2).
prim(my_len2/2).
prim(my_sumlist3/2).
prim(my_head4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_head7/2).
prim(my_max_list8/2).
prim(my_max_list9/2).
prim(my_pred10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
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
p([['g','k','o','t'],['a','t','r','h'],['a','x','h','c'],['d','l','t']],[['g','k','o'],['a','t','r'],['a','x','h'],['d','l']]).
p([['i','n','x'],['n','r','l'],['t','u','s','h']],[['i','n'],['n','r'],['t','u','s']]).
