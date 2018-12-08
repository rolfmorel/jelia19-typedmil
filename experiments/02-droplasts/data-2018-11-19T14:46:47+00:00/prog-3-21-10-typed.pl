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
my_tail1([_|TL],TL).
my_max_list2(A,B):-max_list(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_pred5(A,B):-succ(B,A).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A).
my_succ8(A,B):-succ(A,B).
my_succ9(A,B):-succ(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
my_last17(A,B):-last(A,B).
my_len18(A,B):-length(A,B).
my_succ19(A,B):-succ(A,B).
my_succ20(A,B):-succ(A,B).
prim(my_len0,[list(T),int]).
prim(my_tail1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_len6,[list(T),int]).
prim(my_pred7,[int,int]).
prim(my_succ8,[int,int]).
prim(my_succ9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_len14,[list(T),int]).
prim(my_head15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_last17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_succ19,[int,int]).
prim(my_succ20,[int,int]).
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
p([['s','w','g'],['i','c','p']],[['s','w'],['i','c']]).
p([['f','u','j'],['s','w','b'],['t','t','c','a']],[['f','u'],['s','w'],['t','t','c']]).
