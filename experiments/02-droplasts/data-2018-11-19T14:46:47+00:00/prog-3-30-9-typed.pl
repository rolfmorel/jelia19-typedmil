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
my_min_list0(A,B):-min_list(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_min_list2(A,B):-min_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_succ18(A,B):-succ(A,B).
my_last19(A,B):-last(A,B).
my_reverse20(A,B):-reverse(A,B).
my_min_list21(A,B):-min_list(A,B).
my_max_list22(A,B):-max_list(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_tail25([_|TL],TL).
my_succ26(A,B):-succ(A,B).
my_max_list27(A,B):-max_list(A,B).
my_max_list28(A,B):-max_list(A,B).
my_len29(A,B):-length(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_min_list2,[list(int),int]).
prim(my_min_list3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_max_list5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_max_list12,[list(int),int]).
prim(my_sumlist13,[list(int),int]).
prim(my_min_list14,[list(int),int]).
prim(my_succ15,[int,int]).
prim(my_reverse16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_succ18,[int,int]).
prim(my_last19,[list(T),T]).
prim(my_reverse20,[list(T),T]).
prim(my_min_list21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
prim(my_len23,[list(T),int]).
prim(my_min_list24,[list(int),int]).
prim(my_tail25,[list(T),T]).
prim(my_succ26,[int,int]).
prim(my_max_list27,[list(int),int]).
prim(my_max_list28,[list(int),int]).
prim(my_len29,[list(T),int]).
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
p([['b','s','t','l'],['y','y','m','n'],['w','o','a','k'],['v','b','y','l']],[['b','s','t'],['y','y','m'],['w','o','a'],['v','b','y']]).
p([['c','q','u'],['w','n','g'],['e','w','e']],[['c','q'],['w','n'],['e','w']]).
