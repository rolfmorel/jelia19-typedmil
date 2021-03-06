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
my_max_list1(A,B):-max_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_pred5(A,B):-succ(B,A).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_last9(A,B):-last(A,B).
my_succ10(A,B):-succ(A,B).
my_max_list11(A,B):-max_list(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_pred5,[int,int]).
prim(my_max_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_max_list11,[list(int),int]).
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
p([['o','x','c','u'],['e','r','m'],['f','d','v','b'],['p','a','s','x']],[['o','x','c'],['e','r'],['f','d','v'],['p','a','s']]).
p([['r','b','n'],['s','a','m'],['y','q','q','c'],['j','t','h']],[['r','b'],['s','a'],['y','q','q'],['j','t']]).
