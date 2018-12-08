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
my_tail0([_|TL],TL).
my_min_list1(A,B):-min_list(A,B).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_max_list16(A,B):-max_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_head18([H|_],H).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_max_list21(A,B):-max_list(A,B).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_len24(A,B):-length(A,B).
prim(my_tail0,[list(T),T]).
prim(my_min_list1,[list(int),int]).
prim(my_head2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_len5,[list(T),int]).
prim(my_pred6,[int,int]).
prim(my_pred7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_len10,[list(T),int]).
prim(my_sumlist11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_min_list14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_max_list16,[list(int),int]).
prim(my_min_list17,[list(int),int]).
prim(my_head18,[list(T),T]).
prim(my_tail19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_max_list21,[list(int),int]).
prim(my_len22,[list(T),int]).
prim(my_last23,[list(T),T]).
prim(my_len24,[list(T),int]).
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
p([['d','u','h'],['c','a','u','d']],[['d','u'],['c','a','u']]).
p([['p','a','l'],['g','f','q']],[['p','a'],['g','f']]).
