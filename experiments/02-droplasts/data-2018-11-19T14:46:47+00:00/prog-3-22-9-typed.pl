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
my_max_list0(A,B):-max_list(A,B).
my_last1(A,B):-last(A,B).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_succ6(A,B):-succ(A,B).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_min_list13(A,B):-min_list(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_pred17(A,B):-succ(B,A).
my_tail18([_|TL],TL).
my_len19(A,B):-length(A,B).
my_tail20([_|TL],TL).
my_head21([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_last1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_succ6,[int,int]).
prim(my_max_list7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_last10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_len12,[list(T),int]).
prim(my_min_list13,[list(int),int]).
prim(my_succ14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_pred17,[int,int]).
prim(my_tail18,[list(T),T]).
prim(my_len19,[list(T),int]).
prim(my_tail20,[list(T),T]).
prim(my_head21,[list(T),T]).
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
p([['a','n','r'],['b','x','i'],['y','f','s'],['n','g','w','b']],[['a','n'],['b','x'],['y','f'],['n','g','w']]).
p([['w','a','n','r'],['p','a','d'],['i','e','n','p']],[['w','a','n'],['p','a'],['i','e','n']]).
