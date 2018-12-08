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
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_tail4([_|TL],TL).
my_max_list5(A,B):-max_list(A,B).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_head11([H|_],H).
my_reverse12(A,B):-reverse(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_min_list19(A,B):-min_list(A,B).
my_max_list20(A,B):-max_list(A,B).
my_reverse21(A,B):-reverse(A,B).
my_succ22(A,B):-succ(A,B).
my_reverse23(A,B):-reverse(A,B).
my_pred24(A,B):-succ(B,A).
my_reverse25(A,B):-reverse(A,B).
my_succ26(A,B):-succ(A,B).
my_succ27(A,B):-succ(A,B).
prim(my_pred0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_tail4,[list(T),T]).
prim(my_max_list5,[list(int),int]).
prim(my_last6,[list(T),T]).
prim(my_head7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_head11,[list(T),T]).
prim(my_reverse12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_max_list14,[list(int),int]).
prim(my_min_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_succ18,[int,int]).
prim(my_min_list19,[list(int),int]).
prim(my_max_list20,[list(int),int]).
prim(my_reverse21,[list(T),T]).
prim(my_succ22,[int,int]).
prim(my_reverse23,[list(T),T]).
prim(my_pred24,[int,int]).
prim(my_reverse25,[list(T),T]).
prim(my_succ26,[int,int]).
prim(my_succ27,[int,int]).
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
p([['d','w','r'],['h','o','q']],[['d','w'],['h','o']]).
p([['o','q','l','e'],['v','r','t']],[['o','q','l'],['v','r']]).
