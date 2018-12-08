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
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_min_list8(A,B):-min_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_head18([H|_],H).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_pred21(A,B):-succ(B,A).
my_reverse22(A,B):-reverse(A,B).
my_pred23(A,B):-succ(B,A).
prim(my_tail0,[list(T),T]).
prim(my_tail1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_reverse9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_min_list11,[list(int),int]).
prim(my_max_list12,[list(int),int]).
prim(my_len13,[list(T),int]).
prim(my_min_list14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_min_list16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_head18,[list(T),T]).
prim(my_sumlist19,[list(int),int]).
prim(my_sumlist20,[list(int),int]).
prim(my_pred21,[int,int]).
prim(my_reverse22,[list(T),T]).
prim(my_pred23,[int,int]).
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
p([['t','s','p','y'],['m','x','g']],[['t','s','p'],['m','x']]).
p([['e','u','g','v'],['d','x','m','g'],['l','b','s']],[['e','u','g'],['d','x','m'],['l','b']]).
