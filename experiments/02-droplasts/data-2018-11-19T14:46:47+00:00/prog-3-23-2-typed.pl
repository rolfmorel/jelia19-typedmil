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
my_len0(A,B):-length(A,B).
my_len1(A,B):-length(A,B).
my_tail2([_|TL],TL).
my_pred3(A,B):-succ(B,A).
my_reverse4(A,B):-reverse(A,B).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_reverse8(A,B):-reverse(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
my_head20([H|_],H).
my_last21(A,B):-last(A,B).
my_sumlist22(A,B):-sumlist(A,B).
prim(my_len0,[list(T),int]).
prim(my_len1,[list(T),int]).
prim(my_tail2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_reverse4,[list(T),T]).
prim(my_reverse5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_tail7,[list(T),T]).
prim(my_reverse8,[list(T),T]).
prim(my_reverse9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_reverse11,[list(T),T]).
prim(my_reverse12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_min_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_min_list19,[list(int),int]).
prim(my_head20,[list(T),T]).
prim(my_last21,[list(T),T]).
prim(my_sumlist22,[list(int),int]).
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
p([['i','a','o','s'],['n','t','k','j'],['j','c','m']],[['i','a','o'],['n','t','k'],['j','c']]).
p([['q','r','k'],['w','l','l','n'],['r','y','o']],[['q','r'],['w','l','l'],['r','y']]).
