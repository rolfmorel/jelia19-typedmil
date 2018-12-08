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
my_head0([H|_],H).
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_succ10(A,B):-succ(A,B).
my_reverse11(A,B):-reverse(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_succ14(A,B):-succ(A,B).
my_reverse15(A,B):-reverse(A,B).
my_succ16(A,B):-succ(A,B).
prim(my_head0,[list(T),T]).
prim(my_sumlist1,[list(int),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_len4,[list(T),int]).
prim(my_len5,[list(T),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_reverse9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_reverse11,[list(T),T]).
prim(my_tail12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_succ14,[int,int]).
prim(my_reverse15,[list(T),T]).
prim(my_succ16,[int,int]).
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
p([['o','d','v','h'],['m','l','y','u'],['c','m','q']],[['o','d','v'],['m','l','y'],['c','m']]).
p([['h','w','o'],['u','e','o'],['p','e','y']],[['h','w'],['u','e'],['p','e']]).
