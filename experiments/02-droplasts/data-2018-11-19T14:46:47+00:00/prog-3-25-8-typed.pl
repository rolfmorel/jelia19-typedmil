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
my_min_list1(A,B):-min_list(A,B).
my_last2(A,B):-last(A,B).
my_len3(A,B):-length(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_last10(A,B):-last(A,B).
my_tail11([_|TL],TL).
my_succ12(A,B):-succ(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_tail16([_|TL],TL).
my_head17([H|_],H).
my_min_list18(A,B):-min_list(A,B).
my_len19(A,B):-length(A,B).
my_succ20(A,B):-succ(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_len23(A,B):-length(A,B).
my_pred24(A,B):-succ(B,A).
prim(my_last0,[list(T),T]).
prim(my_min_list1,[list(int),int]).
prim(my_last2,[list(T),T]).
prim(my_len3,[list(T),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(T),int]).
prim(my_last7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_reverse9,[list(T),T]).
prim(my_last10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_sumlist13,[list(int),int]).
prim(my_max_list14,[list(int),int]).
prim(my_max_list15,[list(int),int]).
prim(my_tail16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_min_list18,[list(int),int]).
prim(my_len19,[list(T),int]).
prim(my_succ20,[int,int]).
prim(my_pred21,[int,int]).
prim(my_succ22,[int,int]).
prim(my_len23,[list(T),int]).
prim(my_pred24,[int,int]).
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
p([['w','r','i'],['r','d','k'],['j','h','r']],[['w','r'],['r','d'],['j','h']]).
p([['b','m','n'],['i','v','q','o'],['v','y','a','a']],[['b','m'],['i','v','q'],['v','y','a']]).
