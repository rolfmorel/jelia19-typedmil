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
my_min_list1(A,B):-min_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_min_list8(A,B):-min_list(A,B).
my_last9(A,B):-last(A,B).
my_succ10(A,B):-succ(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_head14([H|_],H).
my_tail15([_|TL],TL).
prim(my_succ0,[int,int]).
prim(my_min_list1,[list(int),int]).
prim(my_min_list2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_min_list8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_len11,[list(T),int]).
prim(my_max_list12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_tail15,[list(T),T]).
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
p([['l','a','x'],['k','j','w'],['d','c','b','v']],[['l','a'],['k','j'],['d','c','b']]).
p([['c','h','n'],['x','k','u'],['j','w','y'],['a','k','u']],[['c','h'],['x','k'],['j','w'],['a','k']]).
