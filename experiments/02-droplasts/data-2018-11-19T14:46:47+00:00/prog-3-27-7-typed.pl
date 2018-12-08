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
my_sumlist0(A,B):-sumlist(A,B).
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_len17(A,B):-length(A,B).
my_head18([H|_],H).
my_last19(A,B):-last(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_pred23(A,B):-succ(B,A).
my_max_list24(A,B):-max_list(A,B).
my_reverse25(A,B):-reverse(A,B).
my_min_list26(A,B):-min_list(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_tail4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_sumlist6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_succ11,[int,int]).
prim(my_reverse12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_tail14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_len17,[list(T),int]).
prim(my_head18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_reverse20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_head22,[list(T),T]).
prim(my_pred23,[int,int]).
prim(my_max_list24,[list(int),int]).
prim(my_reverse25,[list(T),T]).
prim(my_min_list26,[list(int),int]).
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
p([['p','n','a','m'],['r','j','u','p']],[['p','n','a'],['r','j','u']]).
p([['b','x','k'],['i','u','l'],['q','k','i','k']],[['b','x'],['i','u'],['q','k','i']]).
