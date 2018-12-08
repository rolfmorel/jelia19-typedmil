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
my_tail1([_|TL],TL).
my_max_list2(A,B):-max_list(A,B).
my_succ3(A,B):-succ(A,B).
my_len4(A,B):-length(A,B).
my_pred5(A,B):-succ(B,A).
my_min_list6(A,B):-min_list(A,B).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_succ9(A,B):-succ(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_pred14(A,B):-succ(B,A).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_max_list18(A,B):-max_list(A,B).
my_max_list19(A,B):-max_list(A,B).
my_len20(A,B):-length(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_pred23(A,B):-succ(B,A).
my_last24(A,B):-last(A,B).
my_len25(A,B):-length(A,B).
my_len26(A,B):-length(A,B).
my_tail27([_|TL],TL).
my_tail28([_|TL],TL).
my_reverse29(A,B):-reverse(A,B).
prim(my_pred0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_len4,[list(T),int]).
prim(my_pred5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_max_list7,[list(int),int]).
prim(my_min_list8,[list(int),int]).
prim(my_succ9,[int,int]).
prim(my_min_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_reverse12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_tail15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_reverse17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_max_list19,[list(int),int]).
prim(my_len20,[list(T),int]).
prim(my_max_list21,[list(int),int]).
prim(my_min_list22,[list(int),int]).
prim(my_pred23,[int,int]).
prim(my_last24,[list(T),T]).
prim(my_len25,[list(T),int]).
prim(my_len26,[list(T),int]).
prim(my_tail27,[list(T),T]).
prim(my_tail28,[list(T),T]).
prim(my_reverse29,[list(T),T]).
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
p([['m','v','w'],['h','o','t']],[['m','v'],['h','o']]).
p([['c','s','i','x'],['c','d','q','i'],['n','c','e']],[['c','s','i'],['c','d','q'],['n','c']]).
