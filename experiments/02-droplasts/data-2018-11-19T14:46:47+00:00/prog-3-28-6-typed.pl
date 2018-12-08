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
my_reverse1(A,B):-reverse(A,B).
my_succ2(A,B):-succ(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_max_list10(A,B):-max_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_pred15(A,B):-succ(B,A).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_len18(A,B):-length(A,B).
my_succ19(A,B):-succ(A,B).
my_max_list20(A,B):-max_list(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_max_list23(A,B):-max_list(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
my_max_list26(A,B):-max_list(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_succ2,[int,int]).
prim(my_last3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_pred7,[int,int]).
prim(my_len8,[list(T),int]).
prim(my_reverse9,[list(T),T]).
prim(my_max_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_reverse12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_pred15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_succ19,[int,int]).
prim(my_max_list20,[list(int),int]).
prim(my_pred21,[int,int]).
prim(my_succ22,[int,int]).
prim(my_max_list23,[list(int),int]).
prim(my_head24,[list(T),T]).
prim(my_succ25,[int,int]).
prim(my_max_list26,[list(int),int]).
prim(my_reverse27,[list(T),T]).
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
p([['c','y','n'],['x','w','a','e'],['e','r','l'],['i','b','s']],[['c','y'],['x','w','a'],['e','r'],['i','b']]).
p([['l','l','w','g'],['j','k','b','p'],['j','r','q','k'],['k','e','f']],[['l','l','w'],['j','k','b'],['j','r','q'],['k','e']]).
