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
my_min_list1(A,B):-min_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_head3([H|_],H).
my_pred4(A,B):-succ(B,A).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_head13([H|_],H).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_last16(A,B):-last(A,B).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_head19([H|_],H).
prim(my_tail0,[list(T),T]).
prim(my_min_list1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_head3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_max_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_pred12,[int,int]).
prim(my_head13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
prim(my_succ15,[int,int]).
prim(my_last16,[list(T),T]).
prim(my_len17,[list(T),int]).
prim(my_last18,[list(T),T]).
prim(my_head19,[list(T),T]).
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
p([['k','l','n','x'],['o','b','d'],['o','j','i'],['d','b','x','a']],[['k','l','n'],['o','b'],['o','j'],['d','b','x']]).
p([['g','o','l'],['r','b','l','y']],[['g','o'],['r','b','l']]).
