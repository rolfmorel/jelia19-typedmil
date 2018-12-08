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
my_len1(A,B):-length(A,B).
my_last2(A,B):-last(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_len5(A,B):-length(A,B).
prim(my_succ0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_last2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_len5,[list(T),int]).
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
p([['t','j','y'],['l','n','j','p'],['v','n','x','b']],[['t','j'],['l','n','j'],['v','n','x']]).
p([['k','d','i'],['y','e','q','g'],['q','m','e','s'],['u','l','n']],[['k','d'],['y','e','q'],['q','m','e'],['u','l']]).
