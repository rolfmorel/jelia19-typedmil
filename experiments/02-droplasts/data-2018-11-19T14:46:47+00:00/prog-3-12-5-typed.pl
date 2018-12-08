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
my_min_list0(A,B):-min_list(A,B).
my_head1([H|_],H).
my_sumlist2(A,B):-sumlist(A,B).
my_tail3([_|TL],TL).
my_succ4(A,B):-succ(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_min_list7(A,B):-min_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_len9(A,B):-length(A,B).
my_succ10(A,B):-succ(A,B).
my_succ11(A,B):-succ(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_head5,[list(T),T]).
prim(my_reverse6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_min_list8,[list(int),int]).
prim(my_len9,[list(T),int]).
prim(my_succ10,[int,int]).
prim(my_succ11,[int,int]).
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
p([['n','s','b','c'],['u','b','x','h'],['a','h','r','m']],[['n','s','b'],['u','b','x'],['a','h','r']]).
p([['q','x','i'],['c','k','t']],[['q','x'],['c','k']]).
