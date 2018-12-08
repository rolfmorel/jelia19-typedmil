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
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_max_list11(A,B):-max_list(A,B).
my_min_list12(A,B):-min_list(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_tail8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_max_list11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
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
p([['n','r','x','l'],['s','g','c'],['r','x','o']],[['n','r','x'],['s','g'],['r','x']]).
p([['e','d','c'],['a','k','a'],['l','d','l','a'],['t','w','h','x']],[['e','d'],['a','k'],['l','d','l'],['t','w','h']]).
