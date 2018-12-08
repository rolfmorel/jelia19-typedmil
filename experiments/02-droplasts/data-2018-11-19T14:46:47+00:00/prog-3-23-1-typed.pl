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
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A).
my_last8(A,B):-last(A,B).
my_tail9([_|TL],TL).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_tail14([_|TL],TL).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_tail18([_|TL],TL).
my_last19(A,B):-last(A,B).
my_last20(A,B):-last(A,B).
my_succ21(A,B):-succ(A,B).
my_succ22(A,B):-succ(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_tail5,[list(T),T]).
prim(my_last6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_last8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_len10,[list(T),int]).
prim(my_last11,[list(T),T]).
prim(my_tail12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_head15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_min_list17,[list(int),int]).
prim(my_tail18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_last20,[list(T),T]).
prim(my_succ21,[int,int]).
prim(my_succ22,[int,int]).
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
p([['y','a','x'],['y','k','a'],['x','r','x','a'],['y','t','e']],[['y','a'],['y','k'],['x','r','x'],['y','t']]).
p([['d','s','v','y'],['o','e','k'],['k','n','l'],['a','m','s']],[['d','s','v'],['o','e'],['k','n'],['a','m']]).
