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
my_max_list1(A,B):-max_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_tail8([_|TL],TL).
my_head9([H|_],H).
my_max_list10(A,B):-max_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_min_list12(A,B):-min_list(A,B).
my_succ13(A,B):-succ(A,B).
my_last14(A,B):-last(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_tail16([_|TL],TL).
my_max_list17(A,B):-max_list(A,B).
prim(my_succ0,[int,int]).
prim(my_max_list1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_min_list3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_last6,[list(T),T]).
prim(my_head7,[list(T),T]).
prim(my_tail8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_max_list10,[list(int),int]).
prim(my_max_list11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
prim(my_succ13,[int,int]).
prim(my_last14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_tail16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
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
p([['k','l','q'],['u','v','t'],['s','j','r','n']],[['k','l'],['u','v'],['s','j','r']]).
p([['e','j','e'],['s','m','m'],['m','n','p','c']],[['e','j'],['s','m'],['m','n','p']]).
