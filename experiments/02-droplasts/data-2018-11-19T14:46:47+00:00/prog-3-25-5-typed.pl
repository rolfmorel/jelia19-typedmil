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
my_pred0(A,B):-succ(B,A).
my_succ1(A,B):-succ(A,B).
my_reverse2(A,B):-reverse(A,B).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_tail5([_|TL],TL).
my_tail6([_|TL],TL).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_pred11(A,B):-succ(B,A).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_len16(A,B):-length(A,B).
my_succ17(A,B):-succ(A,B).
my_max_list18(A,B):-max_list(A,B).
my_tail19([_|TL],TL).
my_reverse20(A,B):-reverse(A,B).
my_pred21(A,B):-succ(B,A).
my_max_list22(A,B):-max_list(A,B).
my_sumlist23(A,B):-sumlist(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_pred0,[int,int]).
prim(my_succ1,[int,int]).
prim(my_reverse2,[list(T),T]).
prim(my_head3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_tail5,[list(T),T]).
prim(my_tail6,[list(T),T]).
prim(my_last7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_pred11,[int,int]).
prim(my_tail12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_succ14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_succ17,[int,int]).
prim(my_max_list18,[list(int),int]).
prim(my_tail19,[list(T),T]).
prim(my_reverse20,[list(T),T]).
prim(my_pred21,[int,int]).
prim(my_max_list22,[list(int),int]).
prim(my_sumlist23,[list(int),int]).
prim(my_sumlist24,[list(int),int]).
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
p([['m','c','f','x'],['y','b','a'],['b','j','s'],['o','r','c']],[['m','c','f'],['y','b'],['b','j'],['o','r']]).
p([['p','t','k','j'],['h','g','f']],[['p','t','k'],['h','g']]).
