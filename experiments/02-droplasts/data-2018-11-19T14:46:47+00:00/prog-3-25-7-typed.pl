:- use_module('metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail,[list(T),list(T)]).
prim(reverse,[list(T),list(T)]).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(tohigherorder,[P:[list(S),list(T)],Q:[list(S),list(T),[S,T]],F:[S,T]],([P,A,B]:[list(S),list(T)] :- [[Q,A,B,F]:[list(S),list(T),[S,T]]])).
my_succ0(A,B):-succ(A,B).
my_last1(A,B):-last(A,B).
my_pred2(A,B):-succ(B,A).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_len11(A,B):-length(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_pred15(A,B):-succ(B,A).
my_max_list16(A,B):-max_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_head18([H|_],H).
my_succ19(A,B):-succ(A,B).
my_max_list20(A,B):-max_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_tail22([_|TL],TL).
my_head23([H|_],H).
my_max_list24(A,B):-max_list(A,B).
prim(my_succ0,[int,int]).
prim(my_last1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_tail3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_head6,[list(T),T]).
prim(my_last7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_head12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_tail14,[list(T),T]).
prim(my_pred15,[int,int]).
prim(my_max_list16,[list(int),int]).
prim(my_reverse17,[list(T),T]).
prim(my_head18,[list(T),T]).
prim(my_succ19,[int,int]).
prim(my_max_list20,[list(int),int]).
prim(my_max_list21,[list(int),int]).
prim(my_tail22,[list(T),T]).
prim(my_head23,[list(T),T]).
prim(my_max_list24,[list(int),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,[],[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['c','k','s','o'],['i','i','m'],['b','h','r','k']],[['c','k','s'],['i','i'],['b','h','r']]).
p([['o','c','r','j'],['o','o','p'],['q','d','o']],[['o','c','r'],['o','o'],['q','d']]).
