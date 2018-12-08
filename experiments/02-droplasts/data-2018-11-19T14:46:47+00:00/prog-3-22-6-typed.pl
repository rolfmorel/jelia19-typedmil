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
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_last8(A,B):-last(A,B).
my_max_list9(A,B):-max_list(A,B).
my_succ10(A,B):-succ(A,B).
my_pred11(A,B):-succ(B,A).
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_tail18([_|TL],TL).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
prim(my_tail0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_pred6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_last8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_succ10,[int,int]).
prim(my_pred11,[int,int]).
prim(my_reverse12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_len14,[list(T),int]).
prim(my_head15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_sumlist17,[list(int),int]).
prim(my_tail18,[list(T),T]).
prim(my_pred19,[int,int]).
prim(my_pred20,[int,int]).
prim(my_len21,[list(T),int]).
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
p([['x','j','r','j'],['n','q','n'],['x','c','b','i'],['v','y','u']],[['x','j','r'],['n','q'],['x','c','b'],['v','y']]).
p([['w','d','j','i'],['n','o','j'],['a','x','k','y']],[['w','d','j'],['n','o'],['a','x','k']]).
