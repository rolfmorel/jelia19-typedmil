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
my_min_list0(A,B):-min_list(A,B).
my_succ1(A,B):-succ(A,B).
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_last5(A,B):-last(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_len12(A,B):-length(A,B).
my_pred13(A,B):-succ(B,A).
my_reverse14(A,B):-reverse(A,B).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_succ1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_len9,[list(T),int]).
prim(my_succ10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_len12,[list(T),int]).
prim(my_pred13,[int,int]).
prim(my_reverse14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
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
p([['d','d','b'],['h','u','p','q'],['j','r','x']],[['d','d'],['h','u','p'],['j','r']]).
p([['u','g','h'],['i','u','l'],['g','h','k']],[['u','g'],['i','u'],['g','h']]).
