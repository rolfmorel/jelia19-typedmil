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
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
my_tail10([_|TL],TL).
my_reverse11(A,B):-reverse(A,B).
my_tail12([_|TL],TL).
my_pred13(A,B):-succ(B,A).
my_max_list14(A,B):-max_list(A,B).
my_reverse15(A,B):-reverse(A,B).
my_len16(A,B):-length(A,B).
my_min_list17(A,B):-min_list(A,B).
my_tail18([_|TL],TL).
my_last19(A,B):-last(A,B).
my_tail20([_|TL],TL).
my_len21(A,B):-length(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_pred23(A,B):-succ(B,A).
my_pred24(A,B):-succ(B,A).
my_tail25([_|TL],TL).
my_sumlist26(A,B):-sumlist(A,B).
my_max_list27(A,B):-max_list(A,B).
my_pred28(A,B):-succ(B,A).
prim(my_len0,[list(T),int]).
prim(my_min_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_succ3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_len5,[list(T),int]).
prim(my_min_list6,[list(int),int]).
prim(my_tail7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_tail9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_reverse11,[list(T),T]).
prim(my_tail12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_max_list14,[list(int),int]).
prim(my_reverse15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_min_list17,[list(int),int]).
prim(my_tail18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_tail20,[list(T),T]).
prim(my_len21,[list(T),int]).
prim(my_sumlist22,[list(int),int]).
prim(my_pred23,[int,int]).
prim(my_pred24,[int,int]).
prim(my_tail25,[list(T),T]).
prim(my_sumlist26,[list(int),int]).
prim(my_max_list27,[list(int),int]).
prim(my_pred28,[int,int]).
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
p([['p','e','q'],['l','l','s']],[['p','e'],['l','l']]).
p([['b','g','x'],['i','r','b']],[['b','g'],['i','r']]).
