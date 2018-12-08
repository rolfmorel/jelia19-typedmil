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
my_last0(A,B):-last(A,B).
my_last1(A,B):-last(A,B).
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
prim(my_last0,[list(T),T]).
prim(my_last1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_head5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_head7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_tail9,[list(T),T]).
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
p([['y','h','n','l'],['g','o','p','y'],['b','h','c','n'],['p','m','o']],[['y','h','n'],['g','o','p'],['b','h','c'],['p','m']]).
p([['f','u','d','g'],['x','q','p','f']],[['f','u','d'],['x','q','p']]).
