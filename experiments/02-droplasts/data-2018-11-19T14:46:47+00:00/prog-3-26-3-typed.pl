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
my_last1(A,B):-last(A,B).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_tail6([_|TL],TL).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_sumlist13(A,B):-sumlist(A,B).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_succ16(A,B):-succ(A,B).
my_pred17(A,B):-succ(B,A).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
my_pred20(A,B):-succ(B,A).
my_head21([H|_],H).
my_succ22(A,B):-succ(A,B).
my_head23([H|_],H).
my_reverse24(A,B):-reverse(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_head0,[list(T),T]).
prim(my_last1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_head5,[list(T),T]).
prim(my_tail6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_reverse8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_reverse11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_reverse14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_pred17,[int,int]).
prim(my_max_list18,[list(int),int]).
prim(my_min_list19,[list(int),int]).
prim(my_pred20,[int,int]).
prim(my_head21,[list(T),T]).
prim(my_succ22,[int,int]).
prim(my_head23,[list(T),T]).
prim(my_reverse24,[list(T),T]).
prim(my_min_list25,[list(int),int]).
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
p([['l','n','y'],['f','u','k','g'],['c','e','g','g']],[['l','n'],['f','u','k'],['c','e','g']]).
p([['w','m','c','y'],['e','b','b','s'],['j','q','j']],[['w','m','c'],['e','b','b'],['j','q']]).
