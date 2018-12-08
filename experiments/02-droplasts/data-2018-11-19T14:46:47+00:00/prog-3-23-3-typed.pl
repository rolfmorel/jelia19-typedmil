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
my_len2(A,B):-length(A,B).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_min_list10(A,B):-min_list(A,B).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_last16(A,B):-last(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_tail19([_|TL],TL).
my_reverse20(A,B):-reverse(A,B).
my_head21([H|_],H).
my_tail22([_|TL],TL).
prim(my_reverse0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_len2,[list(T),int]).
prim(my_succ3,[int,int]).
prim(my_tail4,[list(T),T]).
prim(my_reverse5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_len8,[list(T),int]).
prim(my_reverse9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_last11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_sumlist13,[list(int),int]).
prim(my_max_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_last16,[list(T),T]).
prim(my_tail17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_tail19,[list(T),T]).
prim(my_reverse20,[list(T),T]).
prim(my_head21,[list(T),T]).
prim(my_tail22,[list(T),T]).
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
p([['p','k','e','f'],['i','l','e'],['y','e','m','m']],[['p','k','e'],['i','l'],['y','e','m']]).
p([['m','b','f','v'],['q','c','i','a'],['m','p','d','q']],[['m','b','f'],['q','c','i'],['m','p','d']]).
