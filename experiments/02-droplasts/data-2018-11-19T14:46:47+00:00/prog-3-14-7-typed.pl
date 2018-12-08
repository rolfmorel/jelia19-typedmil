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
my_pred0(A,B):-succ(B,A).
my_len1(A,B):-length(A,B).
my_reverse2(A,B):-reverse(A,B).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_len12(A,B):-length(A,B).
my_reverse13(A,B):-reverse(A,B).
prim(my_pred0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_reverse2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_min_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_reverse11,[list(T),T]).
prim(my_len12,[list(T),int]).
prim(my_reverse13,[list(T),T]).
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
p([['p','l','q','m'],['k','b','a']],[['p','l','q'],['k','b']]).
p([['c','f','w'],['c','v','w'],['b','t','p'],['b','i','y','a']],[['c','f'],['c','v'],['b','t'],['b','i','y']]).
