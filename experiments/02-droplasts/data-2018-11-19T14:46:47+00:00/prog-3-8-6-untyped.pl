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
my_tail1([_|TL],TL).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
prim(my_len0/2).
prim(my_tail1/2).
prim(my_head2/2).
prim(my_sumlist3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
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
p([['k','w','i'],['f','j','m','k'],['x','x','q'],['t','b','y']],[['k','w'],['f','j','m'],['x','x'],['t','b']]).
p([['s','v','x'],['t','x','t','o']],[['s','v'],['t','x','t']]).
