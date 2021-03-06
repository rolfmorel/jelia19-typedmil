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
my_min_list0(A,B):-min_list(A,B).
my_pred1(A,B):-succ(B,A).
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_pred4(A,B):-succ(B,A).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_tail11([_|TL],TL).
my_head12([H|_],H).
my_max_list13(A,B):-max_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_min_list15(A,B):-min_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_reverse20(A,B):-reverse(A,B).
my_last21(A,B):-last(A,B).
my_reverse22(A,B):-reverse(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_pred1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_pred4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_reverse7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_max_list13,[list(int),int]).
prim(my_sumlist14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_sumlist17,[list(int),int]).
prim(my_len18,[list(T),int]).
prim(my_pred19,[int,int]).
prim(my_reverse20,[list(T),T]).
prim(my_last21,[list(T),T]).
prim(my_reverse22,[list(T),T]).
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
p([['j','e','g'],['g','d','r','w'],['i','j','a','d']],[['j','e'],['g','d','r'],['i','j','a']]).
p([['s','h','b'],['j','q','u'],['e','o','c'],['v','p','r']],[['s','h'],['j','q'],['e','o'],['v','p']]).
