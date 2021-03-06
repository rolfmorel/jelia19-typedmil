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
my_pred0(A,B):-succ(B,A).
my_max_list1(A,B):-max_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
prim(my_pred0,[int,int]).
prim(my_max_list1,[list(int),int]).
prim(my_reverse2,[list(T),T]).
prim(my_reverse3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_tail6,[list(T),T]).
prim(my_reverse7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_pred9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
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
p([['q','w','q','b'],['g','c','o','q'],['b','a','n']],[['q','w','q'],['g','c','o'],['b','a']]).
p([['u','m','b'],['r','b','c'],['x','u','u']],[['u','m'],['r','b'],['x','u']]).
