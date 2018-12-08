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
my_last1(A,B):-last(A,B).
my_reverse2(A,B):-reverse(A,B).
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_last9(A,B):-last(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_reverse19(A,B):-reverse(A,B).
my_max_list20(A,B):-max_list(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_pred23(A,B):-succ(B,A).
my_min_list24(A,B):-min_list(A,B).
my_succ25(A,B):-succ(A,B).
my_succ26(A,B):-succ(A,B).
prim(my_pred0,[int,int]).
prim(my_last1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_max_list5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_last9,[list(T),T]).
prim(my_last10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_min_list12,[list(int),int]).
prim(my_len13,[list(T),int]).
prim(my_head14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_min_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_reverse19,[list(T),T]).
prim(my_max_list20,[list(int),int]).
prim(my_pred21,[int,int]).
prim(my_succ22,[int,int]).
prim(my_pred23,[int,int]).
prim(my_min_list24,[list(int),int]).
prim(my_succ25,[int,int]).
prim(my_succ26,[int,int]).
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
p([['i','s','i','h'],['i','o','f','e'],['d','n','b','c']],[['i','s','i'],['i','o','f'],['d','n','b']]).
p([['j','m','o','l'],['y','l','w'],['y','t','y'],['m','k','c','b']],[['j','m','o'],['y','l'],['y','t'],['m','k','c']]).
