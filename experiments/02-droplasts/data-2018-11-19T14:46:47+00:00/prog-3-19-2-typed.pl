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
my_reverse1(A,B):-reverse(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_pred5(A,B):-succ(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_pred7(A,B):-succ(B,A).
my_max_list8(A,B):-max_list(A,B).
my_min_list9(A,B):-min_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_head11([H|_],H).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_max_list17(A,B):-max_list(A,B).
my_max_list18(A,B):-max_list(A,B).
prim(my_pred0,[int,int]).
prim(my_reverse1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_len3,[list(T),int]).
prim(my_last4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_sumlist6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_min_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_tail12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_max_list17,[list(int),int]).
prim(my_max_list18,[list(int),int]).
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
p([['t','q','l','u'],['i','g','e','n']],[['t','q','l'],['i','g','e']]).
p([['c','a','i'],['t','k','q'],['o','h','m']],[['c','a'],['t','k'],['o','h']]).
