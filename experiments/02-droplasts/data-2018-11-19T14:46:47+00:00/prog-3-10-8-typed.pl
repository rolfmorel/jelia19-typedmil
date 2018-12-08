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
my_succ0(A,B):-succ(A,B).
my_reverse1(A,B):-reverse(A,B).
my_pred2(A,B):-succ(B,A).
my_last3(A,B):-last(A,B).
my_last4(A,B):-last(A,B).
my_pred5(A,B):-succ(B,A).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
prim(my_succ0,[int,int]).
prim(my_reverse1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_last3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_last7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_max_list9,[list(int),int]).
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
p([['m','d','j'],['r','v','c','t'],['d','w','m','b']],[['m','d'],['r','v','c'],['d','w','m']]).
p([['r','e','q','c'],['r','n','w','h'],['m','a','m','d'],['y','i','e']],[['r','e','q'],['r','n','w'],['m','a','m'],['y','i']]).
