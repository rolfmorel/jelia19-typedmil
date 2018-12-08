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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_min_list7(A,B):-min_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_reverse14(A,B):-reverse(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_last22(A,B):-last(A,B).
my_sumlist23(A,B):-sumlist(A,B).
my_tail24([_|TL],TL).
my_tail25([_|TL],TL).
my_succ26(A,B):-succ(A,B).
my_succ27(A,B):-succ(A,B).
my_max_list28(A,B):-max_list(A,B).
my_reverse29(A,B):-reverse(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_min_list7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_reverse14,[list(T),T]).
prim(my_head15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_succ21,[int,int]).
prim(my_last22,[list(T),T]).
prim(my_sumlist23,[list(int),int]).
prim(my_tail24,[list(T),T]).
prim(my_tail25,[list(T),T]).
prim(my_succ26,[int,int]).
prim(my_succ27,[int,int]).
prim(my_max_list28,[list(int),int]).
prim(my_reverse29,[list(T),T]).
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
p([['g','o','u','h'],['x','b','x','s'],['t','k','g','v']],[['g','o','u'],['x','b','x'],['t','k','g']]).
p([['p','s','g','g'],['k','l','m','j'],['t','d','k'],['i','a','j','p']],[['p','s','g'],['k','l','m'],['t','d'],['i','a','j']]).
