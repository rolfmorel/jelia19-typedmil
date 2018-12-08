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
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_pred4(A,B):-succ(B,A).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_tail7([_|TL],TL).
my_last8(A,B):-last(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_head13([H|_],H).
my_sumlist14(A,B):-sumlist(A,B).
my_succ15(A,B):-succ(A,B).
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_tail20([_|TL],TL).
my_pred21(A,B):-succ(B,A).
my_head22([H|_],H).
my_pred23(A,B):-succ(B,A).
my_reverse24(A,B):-reverse(A,B).
my_succ25(A,B):-succ(A,B).
prim(my_succ0,[int,int]).
prim(my_min_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_pred3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_reverse5,[list(T),T]).
prim(my_max_list6,[list(int),int]).
prim(my_tail7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_head13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_succ15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_tail20,[list(T),T]).
prim(my_pred21,[int,int]).
prim(my_head22,[list(T),T]).
prim(my_pred23,[int,int]).
prim(my_reverse24,[list(T),T]).
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
p([['d','v','v'],['q','q','p'],['n','d','s','x'],['w','i','s','u']],[['d','v'],['q','q'],['n','d','s'],['w','i','s']]).
p([['j','s','v','s'],['x','y','o','x']],[['j','s','v'],['x','y','o']]).
