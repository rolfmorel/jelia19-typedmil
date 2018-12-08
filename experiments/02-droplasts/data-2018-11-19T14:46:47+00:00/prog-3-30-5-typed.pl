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
my_len0(A,B):-length(A,B).
my_pred1(A,B):-succ(B,A).
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_reverse5(A,B):-reverse(A,B).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_tail14([_|TL],TL).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
my_succ19(A,B):-succ(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_reverse23(A,B):-reverse(A,B).
my_pred24(A,B):-succ(B,A).
my_len25(A,B):-length(A,B).
my_tail26([_|TL],TL).
my_len27(A,B):-length(A,B).
my_len28(A,B):-length(A,B).
my_tail29([_|TL],TL).
prim(my_len0,[list(T),int]).
prim(my_pred1,[int,int]).
prim(my_max_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_reverse5,[list(T),T]).
prim(my_last6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_succ10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_tail14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_succ16,[int,int]).
prim(my_sumlist17,[list(int),int]).
prim(my_last18,[list(T),T]).
prim(my_succ19,[int,int]).
prim(my_min_list20,[list(int),int]).
prim(my_max_list21,[list(int),int]).
prim(my_min_list22,[list(int),int]).
prim(my_reverse23,[list(T),T]).
prim(my_pred24,[int,int]).
prim(my_len25,[list(T),int]).
prim(my_tail26,[list(T),T]).
prim(my_len27,[list(T),int]).
prim(my_len28,[list(T),int]).
prim(my_tail29,[list(T),T]).
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
p([['r','w','m'],['g','n','x','a']],[['r','w'],['g','n','x']]).
p([['m','g','p','f'],['y','d','p'],['s','m','c','x']],[['m','g','p'],['y','d'],['s','m','c']]).
