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
my_reverse2(A,B):-reverse(A,B).
my_len3(A,B):-length(A,B).
my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_tail8([_|TL],TL).
my_succ9(A,B):-succ(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_succ20(A,B):-succ(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_min_list23(A,B):-min_list(A,B).
my_succ24(A,B):-succ(A,B).
my_min_list25(A,B):-min_list(A,B).
my_reverse26(A,B):-reverse(A,B).
my_head27([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_len3,[list(T),int]).
prim(my_len4,[list(T),int]).
prim(my_max_list5,[list(int),int]).
prim(my_len6,[list(T),int]).
prim(my_min_list7,[list(int),int]).
prim(my_tail8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_head10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_succ14,[int,int]).
prim(my_succ15,[int,int]).
prim(my_reverse16,[list(T),T]).
prim(my_len17,[list(T),int]).
prim(my_last18,[list(T),T]).
prim(my_sumlist19,[list(int),int]).
prim(my_succ20,[int,int]).
prim(my_sumlist21,[list(int),int]).
prim(my_head22,[list(T),T]).
prim(my_min_list23,[list(int),int]).
prim(my_succ24,[int,int]).
prim(my_min_list25,[list(int),int]).
prim(my_reverse26,[list(T),T]).
prim(my_head27,[list(T),T]).
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
p([['k','d','u'],['g','g','s','a']],[['k','d'],['g','g','s']]).
p([['x','l','d','j'],['f','i','w','f'],['n','q','c'],['b','k','g','q']],[['x','l','d'],['f','i','w'],['n','q'],['b','k','g']]).
