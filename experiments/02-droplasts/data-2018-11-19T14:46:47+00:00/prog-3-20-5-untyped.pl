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
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_pred2(A,B):-succ(B,A).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B).
my_last6(A,B):-last(A,B).
my_reverse7(A,B):-reverse(A,B).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_last10(A,B):-last(A,B).
my_head11([H|_],H).
my_reverse12(A,B):-reverse(A,B).
my_min_list13(A,B):-min_list(A,B).
my_head14([H|_],H).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_succ17(A,B):-succ(A,B).
my_reverse18(A,B):-reverse(A,B).
my_tail19([_|TL],TL).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_pred2/2).
prim(my_head3/2).
prim(my_last4/2).
prim(my_succ5/2).
prim(my_last6/2).
prim(my_reverse7/2).
prim(my_max_list8/2).
prim(my_max_list9/2).
prim(my_last10/2).
prim(my_head11/2).
prim(my_reverse12/2).
prim(my_min_list13/2).
prim(my_head14/2).
prim(my_tail15/2).
prim(my_min_list16/2).
prim(my_succ17/2).
prim(my_reverse18/2).
prim(my_tail19/2).
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
p([['x','f','b'],['w','i','s','p'],['j','p','r','f'],['o','j','y','g']],[['x','f'],['w','i','s'],['j','p','r'],['o','j','y']]).
p([['s','o','m','d'],['i','d','k','g'],['u','v','l','a']],[['s','o','m'],['i','d','k'],['u','v','l']]).
