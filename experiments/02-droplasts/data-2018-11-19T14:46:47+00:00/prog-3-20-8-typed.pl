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
my_max_list1(A,B):-max_list(A,B).
my_head2([H|_],H).
my_succ3(A,B):-succ(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_tail8([_|TL],TL).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_last13(A,B):-last(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_len19(A,B):-length(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_max_list1,[list(int),int]).
prim(my_head2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_succ4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(T),int]).
prim(my_succ7,[int,int]).
prim(my_tail8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_last13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_len17,[list(T),int]).
prim(my_last18,[list(T),T]).
prim(my_len19,[list(T),int]).
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
p([['m','b','o','f'],['f','e','v','h']],[['m','b','o'],['f','e','v']]).
p([['i','c','j'],['a','k','j','k']],[['i','c'],['a','k','j']]).
