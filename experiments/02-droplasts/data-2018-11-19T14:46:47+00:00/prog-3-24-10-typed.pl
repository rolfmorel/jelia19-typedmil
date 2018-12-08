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
my_reverse0(A,B):-reverse(A,B).
my_len1(A,B):-length(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_reverse4(A,B):-reverse(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_last7(A,B):-last(A,B).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_last13(A,B):-last(A,B).
my_reverse14(A,B):-reverse(A,B).
my_len15(A,B):-length(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_pred18(A,B):-succ(B,A).
my_sumlist19(A,B):-sumlist(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_min_list23(A,B):-min_list(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_len1,[list(T),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_reverse4,[list(T),T]).
prim(my_head5,[list(T),T]).
prim(my_reverse6,[list(T),T]).
prim(my_last7,[list(T),T]).
prim(my_reverse8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_sumlist10,[list(int),int]).
prim(my_tail11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_last13,[list(T),T]).
prim(my_reverse14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_pred16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_pred18,[int,int]).
prim(my_sumlist19,[list(int),int]).
prim(my_reverse20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
prim(my_min_list23,[list(int),int]).
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
p([['a','b','y','l'],['w','m','o'],['p','m','g'],['o','v','b']],[['a','b','y'],['w','m'],['p','m'],['o','v']]).
p([['g','m','m'],['e','b','s']],[['g','m'],['e','b']]).
