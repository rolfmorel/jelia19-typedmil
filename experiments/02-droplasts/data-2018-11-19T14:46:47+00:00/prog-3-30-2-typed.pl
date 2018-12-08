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
my_min_list1(A,B):-min_list(A,B).
my_pred2(A,B):-succ(B,A).
my_succ3(A,B):-succ(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_succ8(A,B):-succ(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_len18(A,B):-length(A,B).
my_min_list19(A,B):-min_list(A,B).
my_len20(A,B):-length(A,B).
my_min_list21(A,B):-min_list(A,B).
my_len22(A,B):-length(A,B).
my_reverse23(A,B):-reverse(A,B).
my_len24(A,B):-length(A,B).
my_reverse25(A,B):-reverse(A,B).
my_last26(A,B):-last(A,B).
my_len27(A,B):-length(A,B).
my_sumlist28(A,B):-sumlist(A,B).
my_succ29(A,B):-succ(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_min_list1,[list(int),int]).
prim(my_pred2,[int,int]).
prim(my_succ3,[int,int]).
prim(my_max_list4,[list(int),int]).
prim(my_len5,[list(T),int]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_last9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_last11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
prim(my_head15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_min_list19,[list(int),int]).
prim(my_len20,[list(T),int]).
prim(my_min_list21,[list(int),int]).
prim(my_len22,[list(T),int]).
prim(my_reverse23,[list(T),T]).
prim(my_len24,[list(T),int]).
prim(my_reverse25,[list(T),T]).
prim(my_last26,[list(T),T]).
prim(my_len27,[list(T),int]).
prim(my_sumlist28,[list(int),int]).
prim(my_succ29,[int,int]).
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
p([['v','e','w'],['p','n','i','x'],['o','l','e']],[['v','e'],['p','n','i'],['o','l']]).
p([['a','o','g','j'],['c','m','l'],['l','a','n']],[['a','o','g'],['c','m'],['l','a']]).
