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
my_reverse0(A,B):-reverse(A,B).
my_head1([H|_],H).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_reverse11(A,B):-reverse(A,B).
my_max_list12(A,B):-max_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_succ17(A,B):-succ(A,B).
my_tail18([_|TL],TL).
my_head19([H|_],H).
my_tail20([_|TL],TL).
my_sumlist21(A,B):-sumlist(A,B).
my_succ22(A,B):-succ(A,B).
my_succ23(A,B):-succ(A,B).
my_max_list24(A,B):-max_list(A,B).
my_head25([H|_],H).
my_min_list26(A,B):-min_list(A,B).
my_head27([H|_],H).
my_pred28(A,B):-succ(B,A).
my_head29([H|_],H).
prim(my_reverse0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_reverse3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_reverse5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_reverse11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_last15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_succ17,[int,int]).
prim(my_tail18,[list(T),T]).
prim(my_head19,[list(T),T]).
prim(my_tail20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_succ22,[int,int]).
prim(my_succ23,[int,int]).
prim(my_max_list24,[list(int),int]).
prim(my_head25,[list(T),T]).
prim(my_min_list26,[list(int),int]).
prim(my_head27,[list(T),T]).
prim(my_pred28,[int,int]).
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
p([['i','l','d'],['x','s','v','p'],['j','n','x','h'],['d','y','o','e']],[['i','l'],['x','s','v'],['j','n','x'],['d','y','o']]).
p([['d','j','c','a'],['f','e','w','u'],['c','q','q'],['b','c','x','q']],[['d','j','c'],['f','e','w'],['c','q'],['b','c','x']]).
