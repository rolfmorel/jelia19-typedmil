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
my_last0(A,B):-last(A,B).
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_reverse5(A,B):-reverse(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_last12(A,B):-last(A,B).
my_head13([H|_],H).
my_last14(A,B):-last(A,B).
my_tail15([_|TL],TL).
my_pred16(A,B):-succ(B,A).
my_pred17(A,B):-succ(B,A).
my_reverse18(A,B):-reverse(A,B).
prim(my_last0,[list(T),T]).
prim(my_tail1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_sumlist4,[list(int),int]).
prim(my_reverse5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_succ7,[int,int]).
prim(my_len8,[list(T),int]).
prim(my_pred9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_tail11,[list(T),T]).
prim(my_last12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_last14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_pred16,[int,int]).
prim(my_pred17,[int,int]).
prim(my_reverse18,[list(T),T]).
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
p([['p','b','o'],['p','t','q','o'],['p','j','x','k']],[['p','b'],['p','t','q'],['p','j','x']]).
p([['w','m','w'],['m','e','o','p'],['y','o','g'],['d','y','x']],[['w','m'],['m','e','o'],['y','o'],['d','y']]).
