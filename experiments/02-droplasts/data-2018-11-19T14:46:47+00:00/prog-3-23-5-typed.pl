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
my_last1(A,B):-last(A,B).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_succ12(A,B):-succ(A,B).
my_tail13([_|TL],TL).
my_reverse14(A,B):-reverse(A,B).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_tail18([_|TL],TL).
my_len19(A,B):-length(A,B).
my_last20(A,B):-last(A,B).
my_min_list21(A,B):-min_list(A,B).
my_max_list22(A,B):-max_list(A,B).
prim(my_pred0,[int,int]).
prim(my_last1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_succ4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_tail6,[list(T),T]).
prim(my_max_list7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_tail11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_tail13,[list(T),T]).
prim(my_reverse14,[list(T),T]).
prim(my_max_list15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_sumlist17,[list(int),int]).
prim(my_tail18,[list(T),T]).
prim(my_len19,[list(T),int]).
prim(my_last20,[list(T),T]).
prim(my_min_list21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
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
p([['c','j','l','e'],['m','t','f','b'],['y','u','v'],['x','a','o','y']],[['c','j','l'],['m','t','f'],['y','u'],['x','a','o']]).
p([['h','p','r','o'],['b','l','d','c'],['k','m','q'],['f','p','m','b']],[['h','p','r'],['b','l','d'],['k','m'],['f','p','m']]).
