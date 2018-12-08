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
my_reverse0(A,B):-reverse(A,B).
my_head1([H|_],H).
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_last5(A,B):-last(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_last8(A,B):-last(A,B).
my_len9(A,B):-length(A,B).
my_min_list10(A,B):-min_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_last14(A,B):-last(A,B).
my_len15(A,B):-length(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
my_reverse19(A,B):-reverse(A,B).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_tail22([_|TL],TL).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_reverse0/2).
prim(my_head1/2).
prim(my_head2/2).
prim(my_max_list3/2).
prim(my_reverse4/2).
prim(my_last5/2).
prim(my_sumlist6/2).
prim(my_max_list7/2).
prim(my_last8/2).
prim(my_len9/2).
prim(my_min_list10/2).
prim(my_max_list11/2).
prim(my_succ12/2).
prim(my_succ13/2).
prim(my_last14/2).
prim(my_len15/2).
prim(my_min_list16/2).
prim(my_sumlist17/2).
prim(my_last18/2).
prim(my_reverse19/2).
prim(my_min_list20/2).
prim(my_sumlist21/2).
prim(my_tail22/2).
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
p([['f','d','j'],['p','g','h'],['l','v','e'],['f','j','n','a']],[['f','d'],['p','g'],['l','v'],['f','j','n']]).
p([['f','w','s'],['p','m','r'],['t','h','t'],['j','c','j']],[['f','w'],['p','m'],['t','h'],['j','c']]).
