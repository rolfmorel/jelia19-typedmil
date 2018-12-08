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
my_min_list1(A,B):-min_list(A,B).
my_pred2(A,B):-succ(B,A).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_len9(A,B):-length(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_reverse12(A,B):-reverse(A,B).
my_tail13([_|TL],TL).
my_tail14([_|TL],TL).
my_pred15(A,B):-succ(B,A).
my_tail16([_|TL],TL).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_reverse21(A,B):-reverse(A,B).
my_head22([H|_],H).
my_max_list23(A,B):-max_list(A,B).
my_len24(A,B):-length(A,B).
my_last25(A,B):-last(A,B).
my_pred26(A,B):-succ(B,A).
prim(my_head0,[list(T),T]).
prim(my_min_list1,[list(int),int]).
prim(my_pred2,[int,int]).
prim(my_succ3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_reverse7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_len9,[list(T),int]).
prim(my_succ10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_reverse12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_pred15,[int,int]).
prim(my_tail16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_sumlist19,[list(int),int]).
prim(my_sumlist20,[list(int),int]).
prim(my_reverse21,[list(T),T]).
prim(my_head22,[list(T),T]).
prim(my_max_list23,[list(int),int]).
prim(my_len24,[list(T),int]).
prim(my_last25,[list(T),T]).
prim(my_pred26,[int,int]).
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
p([['a','q','a','i'],['t','c','f']],[['a','q','a'],['t','c']]).
p([['g','u','g','p'],['u','u','j','i'],['c','m','p','e'],['v','l','d']],[['g','u','g'],['u','u','j'],['c','m','p'],['v','l']]).
