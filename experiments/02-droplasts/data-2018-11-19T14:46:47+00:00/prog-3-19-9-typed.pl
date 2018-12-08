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
my_max_list1(A,B):-max_list(A,B).
my_tail2([_|TL],TL).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_sumlist17(A,B):-sumlist(A,B).
my_pred18(A,B):-succ(B,A).
prim(my_tail0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_tail2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_tail4,[list(T),T]).
prim(my_len5,[list(T),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_reverse13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_min_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_sumlist17,[list(int),int]).
prim(my_pred18,[int,int]).
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
p([['o','o','p'],['q','l','k'],['l','r','l'],['o','w','r','s']],[['o','o'],['q','l'],['l','r'],['o','w','r']]).
p([['b','x','y'],['g','x','o','m'],['l','i','b','q'],['h','f','e']],[['b','x'],['g','x','o'],['l','i','b'],['h','f']]).
