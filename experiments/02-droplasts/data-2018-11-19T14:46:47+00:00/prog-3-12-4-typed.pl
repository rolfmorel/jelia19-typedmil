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
my_sumlist1(A,B):-sumlist(A,B).
my_max_list2(A,B):-max_list(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_pred6(A,B):-succ(B,A).
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
prim(my_tail0,[list(T),T]).
prim(my_sumlist1,[list(int),int]).
prim(my_max_list2,[list(int),int]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_len7,[list(T),int]).
prim(my_head8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_tail11,[list(T),T]).
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
p([['h','x','a','i'],['o','s','o']],[['h','x','a'],['o','s']]).
p([['f','r','n'],['q','v','i'],['g','h','i','t'],['i','m','w']],[['f','r'],['q','v'],['g','h','i'],['i','m']]).
