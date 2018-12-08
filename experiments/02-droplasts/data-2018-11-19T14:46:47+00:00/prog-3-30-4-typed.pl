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
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_head10([H|_],H).
my_min_list11(A,B):-min_list(A,B).
my_head12([H|_],H).
my_head13([H|_],H).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_succ19(A,B):-succ(A,B).
my_reverse20(A,B):-reverse(A,B).
my_min_list21(A,B):-min_list(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_len24(A,B):-length(A,B).
my_head25([H|_],H).
my_tail26([_|TL],TL).
my_last27(A,B):-last(A,B).
my_min_list28(A,B):-min_list(A,B).
my_head29([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_max_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_pred9,[int,int]).
prim(my_head10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
prim(my_head12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_len14,[list(T),int]).
prim(my_head15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_succ19,[int,int]).
prim(my_reverse20,[list(T),T]).
prim(my_min_list21,[list(int),int]).
prim(my_len22,[list(T),int]).
prim(my_min_list23,[list(int),int]).
prim(my_len24,[list(T),int]).
prim(my_head25,[list(T),T]).
prim(my_tail26,[list(T),T]).
prim(my_last27,[list(T),T]).
prim(my_min_list28,[list(int),int]).
prim(my_head29,[list(T),T]).
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
p([['y','q','t','x'],['d','g','a'],['q','n','e','j'],['a','i','d']],[['y','q','t'],['d','g'],['q','n','e'],['a','i']]).
p([['j','c','q'],['h','b','v'],['j','l','j','x']],[['j','c'],['h','b'],['j','l','j']]).
