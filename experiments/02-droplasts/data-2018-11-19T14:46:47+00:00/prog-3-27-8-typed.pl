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
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_max_list10(A,B):-max_list(A,B).
my_len11(A,B):-length(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_last13(A,B):-last(A,B).
my_succ14(A,B):-succ(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_tail19([_|TL],TL).
my_sumlist20(A,B):-sumlist(A,B).
my_reverse21(A,B):-reverse(A,B).
my_last22(A,B):-last(A,B).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_sumlist25(A,B):-sumlist(A,B).
my_pred26(A,B):-succ(B,A).
prim(my_last0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_min_list4,[list(int),int]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_reverse7,[list(T),T]).
prim(my_len8,[list(T),int]).
prim(my_succ9,[int,int]).
prim(my_max_list10,[list(int),int]).
prim(my_len11,[list(T),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_last13,[list(T),T]).
prim(my_succ14,[int,int]).
prim(my_head15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_tail19,[list(T),T]).
prim(my_sumlist20,[list(int),int]).
prim(my_reverse21,[list(T),T]).
prim(my_last22,[list(T),T]).
prim(my_last23,[list(T),T]).
prim(my_sumlist24,[list(int),int]).
prim(my_sumlist25,[list(int),int]).
prim(my_pred26,[int,int]).
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
p([['v','v','y','v'],['w','k','j'],['h','p','i','w']],[['v','v','y'],['w','k'],['h','p','i']]).
p([['t','f','a'],['b','s','w']],[['t','f'],['b','s']]).
