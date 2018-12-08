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
my_succ1(A,B):-succ(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_succ8(A,B):-succ(A,B).
my_reverse9(A,B):-reverse(A,B).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_head18([H|_],H).
my_reverse19(A,B):-reverse(A,B).
my_last20(A,B):-last(A,B).
prim(my_pred0,[int,int]).
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_succ4,[int,int]).
prim(my_last5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_min_list7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_reverse9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_min_list13,[list(int),int]).
prim(my_len14,[list(T),int]).
prim(my_min_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_head18,[list(T),T]).
prim(my_reverse19,[list(T),T]).
prim(my_last20,[list(T),T]).
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
p([['l','a','l','l'],['w','v','p','g'],['j','b','f','e'],['u','o','p']],[['l','a','l'],['w','v','p'],['j','b','f'],['u','o']]).
p([['h','r','b'],['t','e','g']],[['h','r'],['t','e']]).
