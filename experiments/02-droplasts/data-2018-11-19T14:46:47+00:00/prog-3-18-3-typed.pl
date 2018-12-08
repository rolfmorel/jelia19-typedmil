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
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_head5([H|_],H).
my_sumlist6(A,B):-sumlist(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
prim(my_len0,[list(T),int]).
prim(my_min_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_reverse3,[list(T),T]).
prim(my_len4,[list(T),int]).
prim(my_head5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_reverse9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_min_list13,[list(int),int]).
prim(my_last14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_sumlist17,[list(int),int]).
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
p([['b','s','g','t'],['b','n','d','q'],['y','t','u','q'],['s','m','i']],[['b','s','g'],['b','n','d'],['y','t','u'],['s','m']]).
p([['y','g','v'],['l','u','b','x'],['s','x','l']],[['y','g'],['l','u','b'],['s','x']]).
