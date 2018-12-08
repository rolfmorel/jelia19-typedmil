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
my_head0([H|_],H).
my_last1(A,B):-last(A,B).
my_pred2(A,B):-succ(B,A).
my_sumlist3(A,B):-sumlist(A,B).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_pred6(A,B):-succ(B,A).
my_head7([H|_],H).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_tail10([_|TL],TL).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
prim(my_head0,[list(T),T]).
prim(my_last1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_min_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_pred6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_len8,[list(T),int]).
prim(my_succ9,[int,int]).
prim(my_tail10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_last12,[list(T),T]).
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
p([['g','k','u','o'],['h','p','n'],['u','c','e','r'],['t','v','w','y']],[['g','k','u'],['h','p'],['u','c','e'],['t','v','w']]).
p([['l','n','h','m'],['w','m','g']],[['l','n','h'],['w','m']]).
