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
my_head2([H|_],H).
my_head3([H|_],H).
my_sumlist4(A,B):-sumlist(A,B).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_len7(A,B):-length(A,B).
my_pred8(A,B):-succ(B,A).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_succ17(A,B):-succ(A,B).
my_pred18(A,B):-succ(B,A).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_reverse21(A,B):-reverse(A,B).
my_last22(A,B):-last(A,B).
my_min_list23(A,B):-min_list(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_pred25(A,B):-succ(B,A).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
prim(my_pred0/2).
prim(my_reverse1/2).
prim(my_head2/2).
prim(my_head3/2).
prim(my_sumlist4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_len7/2).
prim(my_pred8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_pred11/2).
prim(my_head12/2).
prim(my_min_list13/2).
prim(my_reverse14/2).
prim(my_sumlist15/2).
prim(my_sumlist16/2).
prim(my_succ17/2).
prim(my_pred18/2).
prim(my_len19/2).
prim(my_pred20/2).
prim(my_reverse21/2).
prim(my_last22/2).
prim(my_min_list23/2).
prim(my_sumlist24/2).
prim(my_pred25/2).
prim(my_head26/2).
prim(my_last27/2).
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
p([['m','c','s','p'],['o','o','t','u'],['k','p','a']],[['m','c','s'],['o','o','t'],['k','p']]).
p([['t','r','l','w'],['b','x','i','r'],['j','f','l','c'],['g','t','h','s']],[['t','r','l'],['b','x','i'],['j','f','l'],['g','t','h']]).
