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
my_len0(A,B):-length(A,B).
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_reverse10(A,B):-reverse(A,B).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B).
my_last13(A,B):-last(A,B).
prim(my_len0,[list(T),int]).
prim(my_pred1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_pred9,[int,int]).
prim(my_reverse10,[list(T),T]).
prim(my_last11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_last13,[list(T),T]).
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
p([['x','g','g','u'],['s','m','x','s'],['j','f','o','y']],[['x','g','g'],['s','m','x'],['j','f','o']]).
p([['u','h','t','q'],['w','q','d'],['k','i','r','x'],['n','u','m','m']],[['u','h','t'],['w','q'],['k','i','r'],['n','u','m']]).
