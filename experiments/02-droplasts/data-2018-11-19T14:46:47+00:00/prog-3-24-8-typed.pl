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
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_pred12(A,B):-succ(B,A).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_min_list23(A,B):-min_list(A,B).
prim(my_pred0,[int,int]).
prim(my_reverse1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_reverse7,[list(T),T]).
prim(my_len8,[list(T),int]).
prim(my_pred9,[int,int]).
prim(my_succ10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_max_list15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_tail17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_head19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
prim(my_sumlist21,[list(int),int]).
prim(my_sumlist22,[list(int),int]).
prim(my_min_list23,[list(int),int]).
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
p([['i','q','n','m'],['w','j','m','n'],['o','t','h','v'],['a','l','c']],[['i','q','n'],['w','j','m'],['o','t','h'],['a','l']]).
p([['y','u','g','l'],['a','y','i'],['w','k','j','x'],['j','d','x','e']],[['y','u','g'],['a','y'],['w','k','j'],['j','d','x']]).
