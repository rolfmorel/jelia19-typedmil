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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_len4(A,B):-length(A,B).
my_pred5(A,B):-succ(B,A).
my_succ6(A,B):-succ(A,B).
my_head7([H|_],H).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_pred19(A,B):-succ(B,A).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_tail22([_|TL],TL).
prim(my_len0,[list(T),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_pred3,[int,int]).
prim(my_len4,[list(T),int]).
prim(my_pred5,[int,int]).
prim(my_succ6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_reverse8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_len13,[list(T),int]).
prim(my_pred14,[int,int]).
prim(my_reverse15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_min_list20,[list(int),int]).
prim(my_sumlist21,[list(int),int]).
prim(my_tail22,[list(T),T]).
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
p([['g','r','l'],['o','b','c','p']],[['g','r'],['o','b','c']]).
p([['b','r','v'],['n','p','p'],['p','v','f','u'],['i','j','f','w']],[['b','r'],['n','p'],['p','v','f'],['i','j','f']]).
