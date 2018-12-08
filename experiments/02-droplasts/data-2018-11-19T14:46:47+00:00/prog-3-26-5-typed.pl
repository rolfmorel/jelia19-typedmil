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
my_reverse1(A,B):-reverse(A,B).
my_pred2(A,B):-succ(B,A).
my_tail3([_|TL],TL).
my_sumlist4(A,B):-sumlist(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_tail7([_|TL],TL).
my_tail8([_|TL],TL).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_min_list12(A,B):-min_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_last14(A,B):-last(A,B).
my_max_list15(A,B):-max_list(A,B).
my_len16(A,B):-length(A,B).
my_len17(A,B):-length(A,B).
my_min_list18(A,B):-min_list(A,B).
my_pred19(A,B):-succ(B,A).
my_max_list20(A,B):-max_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_head23([H|_],H).
my_reverse24(A,B):-reverse(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_succ0,[int,int]).
prim(my_reverse1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_tail3,[list(T),T]).
prim(my_sumlist4,[list(int),int]).
prim(my_tail5,[list(T),T]).
prim(my_last6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_tail8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_len10,[list(T),int]).
prim(my_reverse11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_last14,[list(T),T]).
prim(my_max_list15,[list(int),int]).
prim(my_len16,[list(T),int]).
prim(my_len17,[list(T),int]).
prim(my_min_list18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_max_list20,[list(int),int]).
prim(my_sumlist21,[list(int),int]).
prim(my_head22,[list(T),T]).
prim(my_head23,[list(T),T]).
prim(my_reverse24,[list(T),T]).
prim(my_min_list25,[list(int),int]).
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
p([['b','v','j','b'],['a','k','x'],['m','c','i'],['d','r','m','x']],[['b','v','j'],['a','k'],['m','c'],['d','r','m']]).
p([['w','d','o','r'],['t','q','g','x'],['i','h','c']],[['w','d','o'],['t','q','g'],['i','h']]).
