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
my_tail0([_|TL],TL).
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_len11(A,B):-length(A,B).
my_succ12(A,B):-succ(A,B).
my_last13(A,B):-last(A,B).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_last16(A,B):-last(A,B).
my_succ17(A,B):-succ(A,B).
my_min_list18(A,B):-min_list(A,B).
my_max_list19(A,B):-max_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_last21(A,B):-last(A,B).
my_succ22(A,B):-succ(A,B).
my_len23(A,B):-length(A,B).
my_reverse24(A,B):-reverse(A,B).
my_tail25([_|TL],TL).
my_max_list26(A,B):-max_list(A,B).
my_succ27(A,B):-succ(A,B).
prim(my_tail0,[list(T),T]).
prim(my_tail1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_head3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_len6,[list(T),int]).
prim(my_min_list7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_len11,[list(T),int]).
prim(my_succ12,[int,int]).
prim(my_last13,[list(T),T]).
prim(my_pred14,[int,int]).
prim(my_reverse15,[list(T),T]).
prim(my_last16,[list(T),T]).
prim(my_succ17,[int,int]).
prim(my_min_list18,[list(int),int]).
prim(my_max_list19,[list(int),int]).
prim(my_min_list20,[list(int),int]).
prim(my_last21,[list(T),T]).
prim(my_succ22,[int,int]).
prim(my_len23,[list(T),int]).
prim(my_reverse24,[list(T),T]).
prim(my_tail25,[list(T),T]).
prim(my_max_list26,[list(int),int]).
prim(my_succ27,[int,int]).
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
p([['o','i','i','v'],['k','g','u'],['r','n','u']],[['o','i','i'],['k','g'],['r','n']]).
p([['y','v','m','e'],['f','c','k','m'],['c','p','g']],[['y','v','m'],['f','c','k'],['c','p']]).
