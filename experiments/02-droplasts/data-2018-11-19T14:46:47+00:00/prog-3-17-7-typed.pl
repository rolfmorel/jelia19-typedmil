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
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_tail8([_|TL],TL).
my_succ9(A,B):-succ(A,B).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_reverse12(A,B):-reverse(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A).
my_tail15([_|TL],TL).
my_head16([H|_],H).
prim(my_reverse0,[list(T),T]).
prim(my_pred1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_tail4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_reverse6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_tail8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_len10,[list(T),int]).
prim(my_len11,[list(T),int]).
prim(my_reverse12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_tail15,[list(T),T]).
prim(my_head16,[list(T),T]).
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
p([['u','x','u','f'],['o','m','j']],[['u','x','u'],['o','m']]).
p([['t','c','c','c'],['g','n','q'],['p','o','s','i'],['e','o','v']],[['t','c','c'],['g','n'],['p','o','s'],['e','o']]).
