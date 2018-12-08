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
my_reverse0(A,B):-reverse(A,B).
my_len1(A,B):-length(A,B).
my_len2(A,B):-length(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_last16(A,B):-last(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_head19([H|_],H).
my_len20(A,B):-length(A,B).
my_reverse21(A,B):-reverse(A,B).
my_head22([H|_],H).
my_succ23(A,B):-succ(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_len1,[list(T),int]).
prim(my_len2,[list(T),int]).
prim(my_reverse3,[list(T),T]).
prim(my_len4,[list(T),int]).
prim(my_len5,[list(T),int]).
prim(my_pred6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_pred8,[int,int]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_len13,[list(T),int]).
prim(my_min_list14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_last16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_head19,[list(T),T]).
prim(my_len20,[list(T),int]).
prim(my_reverse21,[list(T),T]).
prim(my_head22,[list(T),T]).
prim(my_succ23,[int,int]).
prim(my_sumlist24,[list(int),int]).
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
p([['b','x','v'],['h','e','o'],['s','c','p','p']],[['b','x'],['h','e'],['s','c','p']]).
p([['o','d','c','r'],['q','g','f','b']],[['o','d','c'],['q','g','f']]).
