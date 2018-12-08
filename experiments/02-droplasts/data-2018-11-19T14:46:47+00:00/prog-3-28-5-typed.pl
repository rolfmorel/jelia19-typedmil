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
my_max_list1(A,B):-max_list(A,B).
my_len2(A,B):-length(A,B).
my_max_list3(A,B):-max_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_max_list11(A,B):-max_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_reverse13(A,B):-reverse(A,B).
my_head14([H|_],H).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_reverse19(A,B):-reverse(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_succ21(A,B):-succ(A,B).
my_reverse22(A,B):-reverse(A,B).
my_max_list23(A,B):-max_list(A,B).
my_tail24([_|TL],TL).
my_len25(A,B):-length(A,B).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
prim(my_pred0,[int,int]).
prim(my_max_list1,[list(int),int]).
prim(my_len2,[list(T),int]).
prim(my_max_list3,[list(int),int]).
prim(my_reverse4,[list(T),T]).
prim(my_len5,[list(T),int]).
prim(my_last6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_reverse12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_head14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_sumlist17,[list(int),int]).
prim(my_len18,[list(T),int]).
prim(my_reverse19,[list(T),T]).
prim(my_sumlist20,[list(int),int]).
prim(my_succ21,[int,int]).
prim(my_reverse22,[list(T),T]).
prim(my_max_list23,[list(int),int]).
prim(my_tail24,[list(T),T]).
prim(my_len25,[list(T),int]).
prim(my_head26,[list(T),T]).
prim(my_last27,[list(T),T]).
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
p([['t','q','g','h'],['w','j','q'],['c','n','v','d'],['m','c','w','k']],[['t','q','g'],['w','j'],['c','n','v'],['m','c','w']]).
p([['f','w','i','m'],['s','o','b','g']],[['f','w','i'],['s','o','b']]).
