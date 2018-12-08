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
my_head0([H|_],H).
my_max_list1(A,B):-max_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_max_list3(A,B):-max_list(A,B).
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_max_list12(A,B):-max_list(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_succ15(A,B):-succ(A,B).
my_pred16(A,B):-succ(B,A).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_min_list20(A,B):-min_list(A,B).
my_succ21(A,B):-succ(A,B).
prim(my_head0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_reverse2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_len5,[list(T),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_tail7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_sumlist9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_reverse11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_succ15,[int,int]).
prim(my_pred16,[int,int]).
prim(my_tail17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
prim(my_succ21,[int,int]).
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
p([['p','l','f'],['r','d','y'],['r','t','b','l']],[['p','l'],['r','d'],['r','t','b']]).
p([['o','h','e'],['m','e','s','r'],['i','x','n'],['b','b','h']],[['o','h'],['m','e','s'],['i','x'],['b','b']]).
