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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_sumlist7(A,B):-sumlist(A,B).
my_succ8(A,B):-succ(A,B).
my_max_list9(A,B):-max_list(A,B).
my_last10(A,B):-last(A,B).
my_reverse11(A,B):-reverse(A,B).
my_last12(A,B):-last(A,B).
my_last13(A,B):-last(A,B).
my_min_list14(A,B):-min_list(A,B).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
prim(my_pred0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_head3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_max_list9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_reverse11,[list(T),T]).
prim(my_last12,[list(T),T]).
prim(my_last13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
prim(my_len15,[list(T),int]).
prim(my_last16,[list(T),T]).
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
p([['v','m','v','b'],['v','l','s','v']],[['v','m','v'],['v','l','s']]).
p([['s','w','a','t'],['k','d','q']],[['s','w','a'],['k','d']]).
