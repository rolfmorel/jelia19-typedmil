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
my_len1(A,B):-length(A,B).
my_max_list2(A,B):-max_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_reverse4(A,B):-reverse(A,B).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_len9(A,B):-length(A,B).
my_tail10([_|TL],TL).
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_min_list13(A,B):-min_list(A,B).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_min_list17(A,B):-min_list(A,B).
my_max_list18(A,B):-max_list(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_max_list20(A,B):-max_list(A,B).
my_head21([H|_],H).
prim(my_head0,[list(T),T]).
prim(my_len1,[list(T),int]).
prim(my_max_list2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_reverse4,[list(T),T]).
prim(my_max_list5,[list(int),int]).
prim(my_len6,[list(T),int]).
prim(my_max_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_len9,[list(T),int]).
prim(my_tail10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
prim(my_min_list13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_min_list17,[list(int),int]).
prim(my_max_list18,[list(int),int]).
prim(my_sumlist19,[list(int),int]).
prim(my_max_list20,[list(int),int]).
prim(my_head21,[list(T),T]).
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
p([['k','i','o'],['q','p','t'],['t','n','a'],['g','j','c']],[['k','i'],['q','p'],['t','n'],['g','j']]).
p([['v','w','n'],['q','m','b'],['m','r','e']],[['v','w'],['q','m'],['m','r']]).
