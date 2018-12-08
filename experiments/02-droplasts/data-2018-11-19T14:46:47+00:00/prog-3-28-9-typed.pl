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
my_max_list0(A,B):-max_list(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_max_list9(A,B):-max_list(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_reverse13(A,B):-reverse(A,B).
my_reverse14(A,B):-reverse(A,B).
my_pred15(A,B):-succ(B,A).
my_min_list16(A,B):-min_list(A,B).
my_pred17(A,B):-succ(B,A).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_min_list23(A,B):-min_list(A,B).
my_pred24(A,B):-succ(B,A).
my_head25([H|_],H).
my_reverse26(A,B):-reverse(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_head3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_len8,[list(T),int]).
prim(my_max_list9,[list(int),int]).
prim(my_head10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_reverse14,[list(T),T]).
prim(my_pred15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_pred17,[int,int]).
prim(my_len18,[list(T),int]).
prim(my_pred19,[int,int]).
prim(my_pred20,[int,int]).
prim(my_sumlist21,[list(int),int]).
prim(my_sumlist22,[list(int),int]).
prim(my_min_list23,[list(int),int]).
prim(my_pred24,[int,int]).
prim(my_head25,[list(T),T]).
prim(my_reverse26,[list(T),T]).
prim(my_reverse27,[list(T),T]).
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
p([['i','d','b'],['j','o','g']],[['i','d'],['j','o']]).
p([['g','c','s','e'],['p','d','j','y'],['s','d','k','b']],[['g','c','s'],['p','d','j'],['s','d','k']]).
