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
my_sumlist1(A,B):-sumlist(A,B).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
prim(my_sumlist0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_head2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_len5,[list(T),int]).
prim(my_pred6,[int,int]).
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
p([['g','h','q','p'],['v','w','w','y'],['w','e','i']],[['g','h','q'],['v','w','w'],['w','e']]).
p([['o','n','y'],['q','a','e'],['r','g','g']],[['o','n'],['q','a'],['r','g']]).
