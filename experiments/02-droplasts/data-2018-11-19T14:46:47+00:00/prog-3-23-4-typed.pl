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
my_head1([H|_],H).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_tail4([_|TL],TL).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_tail10([_|TL],TL).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_pred13(A,B):-succ(B,A).
my_sumlist14(A,B):-sumlist(A,B).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_min_list18(A,B):-min_list(A,B).
my_last19(A,B):-last(A,B).
my_succ20(A,B):-succ(A,B).
my_last21(A,B):-last(A,B).
my_min_list22(A,B):-min_list(A,B).
prim(my_tail0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_tail4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_tail10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_pred13,[int,int]).
prim(my_sumlist14,[list(int),int]).
prim(my_tail15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_min_list18,[list(int),int]).
prim(my_last19,[list(T),T]).
prim(my_succ20,[int,int]).
prim(my_last21,[list(T),T]).
prim(my_min_list22,[list(int),int]).
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
p([['i','i','k'],['d','o','c'],['m','w','w','t']],[['i','i'],['d','o'],['m','w','w']]).
p([['y','j','t'],['s','s','s'],['k','y','o','f'],['k','d','d','a']],[['y','j'],['s','s'],['k','y','o'],['k','d','d']]).
