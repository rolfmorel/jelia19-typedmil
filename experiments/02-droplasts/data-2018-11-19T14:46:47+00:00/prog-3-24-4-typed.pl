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
my_head1([H|_],H).
my_pred2(A,B):-succ(B,A).
my_max_list3(A,B):-max_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_len9(A,B):-length(A,B).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A).
my_min_list13(A,B):-min_list(A,B).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_succ16(A,B):-succ(A,B).
my_reverse17(A,B):-reverse(A,B).
my_min_list18(A,B):-min_list(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_last22(A,B):-last(A,B).
my_sumlist23(A,B):-sumlist(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_max_list3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_last7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_tail10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_min_list13,[list(int),int]).
prim(my_succ14,[int,int]).
prim(my_succ15,[int,int]).
prim(my_succ16,[int,int]).
prim(my_reverse17,[list(T),T]).
prim(my_min_list18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_pred20,[int,int]).
prim(my_succ21,[int,int]).
prim(my_last22,[list(T),T]).
prim(my_sumlist23,[list(int),int]).
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
p([['d','a','s','g'],['j','n','u']],[['d','a','s'],['j','n']]).
p([['x','x','b'],['w','t','p','v'],['l','u','b','m']],[['x','x'],['w','t','p'],['l','u','b']]).
