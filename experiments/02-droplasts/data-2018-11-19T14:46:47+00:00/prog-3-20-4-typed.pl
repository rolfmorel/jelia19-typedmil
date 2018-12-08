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
my_sumlist0(A,B):-sumlist(A,B).
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_last13(A,B):-last(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_pred18(A,B):-succ(B,A).
my_last19(A,B):-last(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_tail1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_tail6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_tail8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_reverse12,[list(T),T]).
prim(my_last13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_reverse15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_reverse17,[list(T),T]).
prim(my_pred18,[int,int]).
prim(my_last19,[list(T),T]).
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
p([['k','e','n'],['r','f','g','p']],[['k','e'],['r','f','g']]).
p([['q','r','l','x'],['a','k','j','k']],[['q','r','l'],['a','k','j']]).
