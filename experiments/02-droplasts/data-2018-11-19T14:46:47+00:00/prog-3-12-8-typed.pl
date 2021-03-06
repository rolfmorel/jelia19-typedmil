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
my_pred1(A,B):-succ(B,A).
my_last2(A,B):-last(A,B).
my_tail3([_|TL],TL).
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_head10([H|_],H).
my_head11([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_pred1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_tail3,[list(T),T]).
prim(my_len4,[list(T),int]).
prim(my_min_list5,[list(int),int]).
prim(my_reverse6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_len8,[list(T),int]).
prim(my_pred9,[int,int]).
prim(my_head10,[list(T),T]).
prim(my_head11,[list(T),T]).
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
p([['i','d','g'],['r','h','y','f'],['o','r','k'],['b','i','p','r']],[['i','d'],['r','h','y'],['o','r'],['b','i','p']]).
p([['c','c','h','i'],['x','r','t']],[['c','c','h'],['x','r']]).
