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
my_succ2(A,B):-succ(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_head5([H|_],H).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_tail9([_|TL],TL).
my_succ10(A,B):-succ(A,B).
my_reverse11(A,B):-reverse(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_succ13(A,B):-succ(A,B).
my_head14([H|_],H).
prim(my_sumlist0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_succ4,[int,int]).
prim(my_head5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_reverse11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_succ13,[int,int]).
prim(my_head14,[list(T),T]).
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
p([['c','o','x'],['n','a','l'],['w','d','u']],[['c','o'],['n','a'],['w','d']]).
p([['r','i','f','y'],['a','i','o','n'],['p','u','s','d'],['f','r','k']],[['r','i','f'],['a','i','o'],['p','u','s'],['f','r']]).
