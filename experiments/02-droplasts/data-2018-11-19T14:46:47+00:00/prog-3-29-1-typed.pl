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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_succ16(A,B):-succ(A,B).
my_last17(A,B):-last(A,B).
my_min_list18(A,B):-min_list(A,B).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
my_last22(A,B):-last(A,B).
my_len23(A,B):-length(A,B).
my_last24(A,B):-last(A,B).
my_len25(A,B):-length(A,B).
my_pred26(A,B):-succ(B,A).
my_len27(A,B):-length(A,B).
my_max_list28(A,B):-max_list(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_last12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_max_list15,[list(int),int]).
prim(my_succ16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_min_list18,[list(int),int]).
prim(my_tail19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_len21,[list(T),int]).
prim(my_last22,[list(T),T]).
prim(my_len23,[list(T),int]).
prim(my_last24,[list(T),T]).
prim(my_len25,[list(T),int]).
prim(my_pred26,[int,int]).
prim(my_len27,[list(T),int]).
prim(my_max_list28,[list(int),int]).
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
p([['j','m','a'],['w','e','c','x'],['c','l','y'],['u','f','v','x']],[['j','m'],['w','e','c'],['c','l'],['u','f','v']]).
p([['p','h','c'],['h','k','r'],['b','k','d'],['l','h','u','r']],[['p','h'],['h','k'],['b','k'],['l','h','u']]).
