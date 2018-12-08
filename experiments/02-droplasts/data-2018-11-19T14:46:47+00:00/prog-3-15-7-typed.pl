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
my_max_list1(A,B):-max_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A).
my_pred8(A,B):-succ(B,A).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_pred14(A,B):-succ(B,A).
prim(my_min_list0,[list(int),int]).
prim(my_max_list1,[list(int),int]).
prim(my_min_list2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_reverse4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_pred8,[int,int]).
prim(my_pred9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_max_list11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_pred14,[int,int]).
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
p([['n','m','a','y'],['h','m','h'],['y','m','y','q']],[['n','m','a'],['h','m'],['y','m','y']]).
p([['x','m','d'],['w','r','d','x']],[['x','m'],['w','r','d']]).
