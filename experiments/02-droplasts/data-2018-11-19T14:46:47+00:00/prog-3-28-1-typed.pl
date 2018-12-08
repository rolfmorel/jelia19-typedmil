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
my_last2(A,B):-last(A,B).
my_min_list3(A,B):-min_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_len12(A,B):-length(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_len14(A,B):-length(A,B).
my_pred15(A,B):-succ(B,A).
my_pred16(A,B):-succ(B,A).
my_reverse17(A,B):-reverse(A,B).
my_pred18(A,B):-succ(B,A).
my_max_list19(A,B):-max_list(A,B).
my_head20([H|_],H).
my_succ21(A,B):-succ(A,B).
my_len22(A,B):-length(A,B).
my_tail23([_|TL],TL).
my_pred24(A,B):-succ(B,A).
my_last25(A,B):-last(A,B).
my_sumlist26(A,B):-sumlist(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_pred0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_tail8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_tail11,[list(T),T]).
prim(my_len12,[list(T),int]).
prim(my_sumlist13,[list(int),int]).
prim(my_len14,[list(T),int]).
prim(my_pred15,[int,int]).
prim(my_pred16,[int,int]).
prim(my_reverse17,[list(T),T]).
prim(my_pred18,[int,int]).
prim(my_max_list19,[list(int),int]).
prim(my_head20,[list(T),T]).
prim(my_succ21,[int,int]).
prim(my_len22,[list(T),int]).
prim(my_tail23,[list(T),T]).
prim(my_pred24,[int,int]).
prim(my_last25,[list(T),T]).
prim(my_sumlist26,[list(int),int]).
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
p([['l','x','w','x'],['j','n','q','h'],['e','w','f','g'],['s','n','c','o']],[['l','x','w'],['j','n','q'],['e','w','f'],['s','n','c']]).
p([['a','p','d'],['o','q','e'],['o','y','y','d'],['b','e','g','t']],[['a','p'],['o','q'],['o','y','y'],['b','e','g']]).
