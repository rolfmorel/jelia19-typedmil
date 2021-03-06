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
my_sumlist1(A,B):-sumlist(A,B).
my_reverse2(A,B):-reverse(A,B).
my_min_list3(A,B):-min_list(A,B).
my_succ4(A,B):-succ(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A).
my_min_list11(A,B):-min_list(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
prim(my_max_list0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_reverse2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_succ4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_reverse7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_min_list11,[list(int),int]).
prim(my_head12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_tail14,[list(T),T]).
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
p([['d','k','s'],['i','r','v'],['n','i','a'],['v','h','w']],[['d','k'],['i','r'],['n','i'],['v','h']]).
p([['r','n','c'],['v','n','e','m']],[['r','n'],['v','n','e']]).
