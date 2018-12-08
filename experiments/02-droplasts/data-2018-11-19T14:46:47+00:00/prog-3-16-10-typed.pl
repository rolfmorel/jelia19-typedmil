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
my_max_list0(A,B):-max_list(A,B).
my_last1(A,B):-last(A,B).
my_len2(A,B):-length(A,B).
my_head3([H|_],H).
my_len4(A,B):-length(A,B).
my_last5(A,B):-last(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_pred8(A,B):-succ(B,A).
my_last9(A,B):-last(A,B).
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_last1,[list(T),T]).
prim(my_len2,[list(T),int]).
prim(my_head3,[list(T),T]).
prim(my_len4,[list(T),int]).
prim(my_last5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_len7,[list(T),int]).
prim(my_pred8,[int,int]).
prim(my_last9,[list(T),T]).
prim(my_reverse10,[list(T),T]).
prim(my_succ11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
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
p([['p','x','e'],['w','j','s','v'],['f','c','i','s']],[['p','x'],['w','j','s'],['f','c','i']]).
p([['m','s','a'],['s','n','i','o'],['x','j','x']],[['m','s'],['s','n','i'],['x','j']]).
