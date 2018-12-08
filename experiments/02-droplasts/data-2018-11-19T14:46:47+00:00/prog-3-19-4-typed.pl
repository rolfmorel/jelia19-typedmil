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
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_pred3(A,B):-succ(B,A).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_min_list7(A,B):-min_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_last13(A,B):-last(A,B).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_sumlist16(A,B):-sumlist(A,B).
my_pred17(A,B):-succ(B,A).
my_head18([H|_],H).
prim(my_len0,[list(T),int]).
prim(my_pred1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_pred3,[int,int]).
prim(my_min_list4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_tail6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_min_list8,[list(int),int]).
prim(my_tail9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_last13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_tail15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_pred17,[int,int]).
prim(my_head18,[list(T),T]).
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
p([['q','x','k','v'],['m','b','v'],['j','r','g']],[['q','x','k'],['m','b'],['j','r']]).
p([['d','b','h','t'],['a','g','y'],['g','x','r','y'],['p','q','n','q']],[['d','b','h'],['a','g'],['g','x','r'],['p','q','n']]).
