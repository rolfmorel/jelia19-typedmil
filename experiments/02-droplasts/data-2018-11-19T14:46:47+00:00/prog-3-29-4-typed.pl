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
my_max_list1(A,B):-max_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B).
my_tail5([_|TL],TL).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_head13([H|_],H).
my_last14(A,B):-last(A,B).
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_pred21(A,B):-succ(B,A).
my_pred22(A,B):-succ(B,A).
my_len23(A,B):-length(A,B).
my_pred24(A,B):-succ(B,A).
my_sumlist25(A,B):-sumlist(A,B).
my_succ26(A,B):-succ(A,B).
my_max_list27(A,B):-max_list(A,B).
my_min_list28(A,B):-min_list(A,B).
prim(my_tail0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_tail5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_sumlist9,[list(int),int]).
prim(my_len10,[list(T),int]).
prim(my_sumlist11,[list(int),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_head13,[list(T),T]).
prim(my_last14,[list(T),T]).
prim(my_head15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_tail17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_head19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
prim(my_pred21,[int,int]).
prim(my_pred22,[int,int]).
prim(my_len23,[list(T),int]).
prim(my_pred24,[int,int]).
prim(my_sumlist25,[list(int),int]).
prim(my_succ26,[int,int]).
prim(my_max_list27,[list(int),int]).
prim(my_min_list28,[list(int),int]).
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
p([['m','d','i'],['c','r','w','h']],[['m','d'],['c','r','w']]).
p([['w','g','i'],['h','m','r','j'],['k','v','m','d'],['o','r','m']],[['w','g'],['h','m','r'],['k','v','m'],['o','r']]).
