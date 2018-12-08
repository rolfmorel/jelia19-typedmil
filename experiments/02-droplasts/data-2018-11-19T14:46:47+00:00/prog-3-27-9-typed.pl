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
my_succ0(A,B):-succ(A,B).
my_max_list1(A,B):-max_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_head3([H|_],H).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_head7([H|_],H).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_head13([H|_],H).
my_sumlist14(A,B):-sumlist(A,B).
my_head15([H|_],H).
my_last16(A,B):-last(A,B).
my_max_list17(A,B):-max_list(A,B).
my_last18(A,B):-last(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_pred20(A,B):-succ(B,A).
my_min_list21(A,B):-min_list(A,B).
my_head22([H|_],H).
my_sumlist23(A,B):-sumlist(A,B).
my_pred24(A,B):-succ(B,A).
my_reverse25(A,B):-reverse(A,B).
my_head26([H|_],H).
prim(my_succ0,[int,int]).
prim(my_max_list1,[list(int),int]).
prim(my_reverse2,[list(T),T]).
prim(my_head3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_len8,[list(T),int]).
prim(my_succ9,[int,int]).
prim(my_last10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_min_list12,[list(int),int]).
prim(my_head13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_head15,[list(T),T]).
prim(my_last16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_last18,[list(T),T]).
prim(my_sumlist19,[list(int),int]).
prim(my_pred20,[int,int]).
prim(my_min_list21,[list(int),int]).
prim(my_head22,[list(T),T]).
prim(my_sumlist23,[list(int),int]).
prim(my_pred24,[int,int]).
prim(my_reverse25,[list(T),T]).
prim(my_head26,[list(T),T]).
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
p([['q','e','u','s'],['c','f','p'],['v','s','t','j'],['r','v','w']],[['q','e','u'],['c','f'],['v','s','t'],['r','v']]).
p([['s','d','u'],['u','c','x','f'],['o','c','p','i'],['u','o','b']],[['s','d'],['u','c','x'],['o','c','p'],['u','o']]).
