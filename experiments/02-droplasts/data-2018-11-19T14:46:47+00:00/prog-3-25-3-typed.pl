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
my_min_list1(A,B):-min_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_len3(A,B):-length(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_tail10([_|TL],TL).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_succ13(A,B):-succ(A,B).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_succ18(A,B):-succ(A,B).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_pred23(A,B):-succ(B,A).
my_last24(A,B):-last(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_min_list1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_len3,[list(T),int]).
prim(my_head4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_len8,[list(T),int]).
prim(my_succ9,[int,int]).
prim(my_tail10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_succ13,[int,int]).
prim(my_succ14,[int,int]).
prim(my_succ15,[int,int]).
prim(my_reverse16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_succ18,[int,int]).
prim(my_tail19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_sumlist21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
prim(my_pred23,[int,int]).
prim(my_last24,[list(T),T]).
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
p([['x','u','j'],['s','b','c'],['u','b','s'],['p','p','u']],[['x','u'],['s','b'],['u','b'],['p','p']]).
p([['y','n','r'],['q','w','u','i'],['a','a','t']],[['y','n'],['q','w','u'],['a','a']]).
