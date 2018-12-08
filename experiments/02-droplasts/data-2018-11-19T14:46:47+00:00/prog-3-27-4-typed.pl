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
my_last2(A,B):-last(A,B).
my_reverse3(A,B):-reverse(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_min_list6(A,B):-min_list(A,B).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_max_list10(A,B):-max_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_pred19(A,B):-succ(B,A).
my_len20(A,B):-length(A,B).
my_succ21(A,B):-succ(A,B).
my_len22(A,B):-length(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_min_list25(A,B):-min_list(A,B).
my_min_list26(A,B):-min_list(A,B).
prim(my_last0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_reverse3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_pred8,[int,int]).
prim(my_reverse9,[list(T),T]).
prim(my_max_list10,[list(int),int]).
prim(my_max_list11,[list(int),int]).
prim(my_head12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_len14,[list(T),int]).
prim(my_last15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_len20,[list(T),int]).
prim(my_succ21,[int,int]).
prim(my_len22,[list(T),int]).
prim(my_len23,[list(T),int]).
prim(my_min_list24,[list(int),int]).
prim(my_min_list25,[list(int),int]).
prim(my_min_list26,[list(int),int]).
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
p([['x','w','r'],['i','h','q','i'],['r','u','n','p']],[['x','w'],['i','h','q'],['r','u','n']]).
p([['w','w','h','v'],['j','b','x'],['j','y','p'],['y','t','e','x']],[['w','w','h'],['j','b'],['j','y'],['y','t','e']]).
