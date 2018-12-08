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
my_last0(A,B):-last(A,B).
my_len1(A,B):-length(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_last13(A,B):-last(A,B).
my_tail14([_|TL],TL).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_pred17(A,B):-succ(B,A).
my_max_list18(A,B):-max_list(A,B).
my_pred19(A,B):-succ(B,A).
my_succ20(A,B):-succ(A,B).
my_pred21(A,B):-succ(B,A).
my_max_list22(A,B):-max_list(A,B).
my_succ23(A,B):-succ(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
prim(my_last0,[list(T),T]).
prim(my_len1,[list(T),int]).
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_head7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_head9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_last13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_pred17,[int,int]).
prim(my_max_list18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_succ20,[int,int]).
prim(my_pred21,[int,int]).
prim(my_max_list22,[list(int),int]).
prim(my_succ23,[int,int]).
prim(my_head24,[list(T),T]).
prim(my_succ25,[int,int]).
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
p([['o','w','m','i'],['c','y','n','w'],['g','b','l','k'],['n','b','x']],[['o','w','m'],['c','y','n'],['g','b','l'],['n','b']]).
p([['x','h','y','t'],['m','l','r','c']],[['x','h','y'],['m','l','r']]).
