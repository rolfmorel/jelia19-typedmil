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
my_last0(A,B):-last(A,B).
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_tail6([_|TL],TL).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_pred13(A,B):-succ(B,A).
my_last14(A,B):-last(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_reverse18(A,B):-reverse(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_tail20([_|TL],TL).
prim(my_last0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_tail6,[list(T),T]).
prim(my_succ7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_sumlist11,[list(int),int]).
prim(my_len12,[list(T),int]).
prim(my_pred13,[int,int]).
prim(my_last14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_sumlist16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_sumlist19,[list(int),int]).
prim(my_tail20,[list(T),T]).
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
p([['n','s','b','g'],['q','x','u','a'],['t','n','g']],[['n','s','b'],['q','x','u'],['t','n']]).
p([['f','t','c'],['e','o','m'],['r','r','k','o'],['g','t','x','t']],[['f','t'],['e','o'],['r','r','k'],['g','t','x']]).
