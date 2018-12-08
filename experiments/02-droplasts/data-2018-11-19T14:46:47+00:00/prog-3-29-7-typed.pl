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
my_tail1([_|TL],TL).
my_tail2([_|TL],TL).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_head12([H|_],H).
my_pred13(A,B):-succ(B,A).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B).
my_min_list17(A,B):-min_list(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_head19([H|_],H).
my_last20(A,B):-last(A,B).
my_reverse21(A,B):-reverse(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
my_succ26(A,B):-succ(A,B).
my_tail27([_|TL],TL).
my_reverse28(A,B):-reverse(A,B).
prim(my_succ0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_tail2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_pred6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_pred8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_max_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_succ16,[int,int]).
prim(my_min_list17,[list(int),int]).
prim(my_sumlist18,[list(int),int]).
prim(my_head19,[list(T),T]).
prim(my_last20,[list(T),T]).
prim(my_reverse21,[list(T),T]).
prim(my_len22,[list(T),int]).
prim(my_min_list23,[list(int),int]).
prim(my_head24,[list(T),T]).
prim(my_succ25,[int,int]).
prim(my_succ26,[int,int]).
prim(my_tail27,[list(T),T]).
prim(my_reverse28,[list(T),T]).
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
p([['k','x','p'],['n','l','b']],[['k','x'],['n','l']]).
p([['c','c','m'],['q','c','j'],['y','n','v']],[['c','c'],['q','c'],['y','n']]).
