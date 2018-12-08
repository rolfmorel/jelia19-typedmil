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
my_pred1(A,B):-succ(B,A).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
my_succ13(A,B):-succ(A,B).
my_min_list14(A,B):-min_list(A,B).
my_tail15([_|TL],TL).
prim(my_tail0,[list(T),T]).
prim(my_pred1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_sumlist4,[list(int),int]).
prim(my_last5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_len11,[list(T),int]).
prim(my_last12,[list(T),T]).
prim(my_succ13,[int,int]).
prim(my_min_list14,[list(int),int]).
prim(my_tail15,[list(T),T]).
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
p([['i','o','x'],['l','y','c','t'],['t','h','p','y'],['u','b','j']],[['i','o'],['l','y','c'],['t','h','p'],['u','b']]).
p([['c','t','x'],['s','u','k'],['l','m','m','n']],[['c','t'],['s','u'],['l','m','m']]).
