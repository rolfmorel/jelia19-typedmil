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
my_tail1([_|TL],TL).
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_tail4([_|TL],TL).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_succ10(A,B):-succ(A,B).
my_head11([H|_],H).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_reverse14(A,B):-reverse(A,B).
prim(my_succ0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_succ2,[int,int]).
prim(my_min_list3,[list(int),int]).
prim(my_tail4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_last6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_len8,[list(T),int]).
prim(my_last9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_head11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_reverse14,[list(T),T]).
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
p([['u','m','j'],['h','f','a'],['w','q','y'],['b','g','p']],[['u','m'],['h','f'],['w','q'],['b','g']]).
p([['i','b','u'],['s','l','a','x'],['q','f','i'],['w','j','y','h']],[['i','b'],['s','l','a'],['q','f'],['w','j','y']]).
