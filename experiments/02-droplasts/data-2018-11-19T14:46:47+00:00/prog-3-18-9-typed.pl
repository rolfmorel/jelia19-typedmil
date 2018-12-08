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
my_tail0([_|TL],TL).
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_tail12([_|TL],TL).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_min_list17(A,B):-min_list(A,B).
prim(my_tail0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_head3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_max_list15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_min_list17,[list(int),int]).
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
p([['o','a','s','w'],['l','p','x','k'],['u','a','i'],['b','m','p']],[['o','a','s'],['l','p','x'],['u','a'],['b','m']]).
p([['t','w','p','l'],['l','h','y','q'],['q','m','b']],[['t','w','p'],['l','h','y'],['q','m']]).
