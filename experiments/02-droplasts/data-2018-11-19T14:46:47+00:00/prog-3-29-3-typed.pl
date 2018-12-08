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
my_len1(A,B):-length(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_pred8(A,B):-succ(B,A).
my_len9(A,B):-length(A,B).
my_min_list10(A,B):-min_list(A,B).
my_pred11(A,B):-succ(B,A).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_tail17([_|TL],TL).
my_pred18(A,B):-succ(B,A).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_min_list22(A,B):-min_list(A,B).
my_succ23(A,B):-succ(A,B).
my_pred24(A,B):-succ(B,A).
my_len25(A,B):-length(A,B).
my_succ26(A,B):-succ(A,B).
my_tail27([_|TL],TL).
my_pred28(A,B):-succ(B,A).
prim(my_pred0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_max_list7,[list(int),int]).
prim(my_pred8,[int,int]).
prim(my_len9,[list(T),int]).
prim(my_min_list10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_tail12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_tail14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_tail17,[list(T),T]).
prim(my_pred18,[int,int]).
prim(my_len19,[list(T),int]).
prim(my_pred20,[int,int]).
prim(my_succ21,[int,int]).
prim(my_min_list22,[list(int),int]).
prim(my_succ23,[int,int]).
prim(my_pred24,[int,int]).
prim(my_len25,[list(T),int]).
prim(my_succ26,[int,int]).
prim(my_tail27,[list(T),T]).
prim(my_pred28,[int,int]).
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
p([['i','b','r','c'],['t','h','s','j']],[['i','b','r'],['t','h','s']]).
p([['f','m','g'],['r','x','m','h'],['a','e','o','s'],['g','n','s','e']],[['f','m'],['r','x','m'],['a','e','o'],['g','n','s']]).
