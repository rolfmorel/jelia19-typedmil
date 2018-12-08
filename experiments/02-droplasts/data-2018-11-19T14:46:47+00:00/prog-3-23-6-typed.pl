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
my_min_list0(A,B):-min_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_last7(A,B):-last(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B).
my_len12(A,B):-length(A,B).
my_len13(A,B):-length(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_reverse17(A,B):-reverse(A,B).
my_len18(A,B):-length(A,B).
my_len19(A,B):-length(A,B).
my_succ20(A,B):-succ(A,B).
my_min_list21(A,B):-min_list(A,B).
my_head22([H|_],H).
prim(my_min_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_len5,[list(T),int]).
prim(my_min_list6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_reverse8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_reverse10,[list(T),T]).
prim(my_succ11,[int,int]).
prim(my_len12,[list(T),int]).
prim(my_len13,[list(T),int]).
prim(my_succ14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_reverse17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_len19,[list(T),int]).
prim(my_succ20,[int,int]).
prim(my_min_list21,[list(int),int]).
prim(my_head22,[list(T),T]).
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
p([['o','l','w','a'],['h','y','r','l']],[['o','l','w'],['h','y','r']]).
p([['u','h','k','x'],['y','h','o','x'],['j','q','e']],[['u','h','k'],['y','h','o'],['j','q']]).
