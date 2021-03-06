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
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_succ4(A,B):-succ(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_reverse14(A,B):-reverse(A,B).
my_tail15([_|TL],TL).
prim(my_tail0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_reverse3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_len5,[list(T),int]).
prim(my_min_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_reverse14,[list(T),T]).
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
p([['g','d','x'],['r','t','d','i'],['h','u','l','v']],[['g','d'],['r','t','d'],['h','u','l']]).
p([['k','v','b'],['k','a','a'],['t','s','g','q']],[['k','v'],['k','a'],['t','s','g']]).
