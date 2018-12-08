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
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_succ10(A,B):-succ(A,B).
my_tail11([_|TL],TL).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_head16([H|_],H).
my_max_list17(A,B):-max_list(A,B).
my_reverse18(A,B):-reverse(A,B).
my_min_list19(A,B):-min_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_max_list23(A,B):-max_list(A,B).
my_succ24(A,B):-succ(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_len7,[list(T),int]).
prim(my_min_list8,[list(int),int]).
prim(my_sumlist9,[list(int),int]).
prim(my_succ10,[int,int]).
prim(my_tail11,[list(T),T]).
prim(my_last12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_min_list14,[list(int),int]).
prim(my_succ15,[int,int]).
prim(my_head16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_reverse18,[list(T),T]).
prim(my_min_list19,[list(int),int]).
prim(my_reverse20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
prim(my_max_list23,[list(int),int]).
prim(my_succ24,[int,int]).
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
p([['v','d','q','x'],['p','a','l'],['u','v','x'],['c','v','f']],[['v','d','q'],['p','a'],['u','v'],['c','v']]).
p([['m','n','x'],['w','k','y','v']],[['m','n'],['w','k','y']]).
