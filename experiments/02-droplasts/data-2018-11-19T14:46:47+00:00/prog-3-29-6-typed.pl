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
my_pred0(A,B):-succ(B,A).
my_head1([H|_],H).
my_max_list2(A,B):-max_list(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_head21([H|_],H).
my_min_list22(A,B):-min_list(A,B).
my_len23(A,B):-length(A,B).
my_len24(A,B):-length(A,B).
my_tail25([_|TL],TL).
my_max_list26(A,B):-max_list(A,B).
my_pred27(A,B):-succ(B,A).
my_head28([H|_],H).
prim(my_pred0,[int,int]).
prim(my_head1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_succ14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_len18,[list(T),int]).
prim(my_head19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
prim(my_head21,[list(T),T]).
prim(my_min_list22,[list(int),int]).
prim(my_len23,[list(T),int]).
prim(my_len24,[list(T),int]).
prim(my_tail25,[list(T),T]).
prim(my_max_list26,[list(int),int]).
prim(my_pred27,[int,int]).
prim(my_head28,[list(T),T]).
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
p([['n','c','r','f'],['r','k','l']],[['n','c','r'],['r','k']]).
p([['v','d','i','l'],['c','m','s'],['i','q','g']],[['v','d','i'],['c','m'],['i','q']]).
