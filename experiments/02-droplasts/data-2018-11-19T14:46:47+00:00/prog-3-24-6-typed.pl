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
my_tail0([_|TL],TL).
my_succ1(A,B):-succ(A,B).
my_max_list2(A,B):-max_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_pred4(A,B):-succ(B,A).
my_pred5(A,B):-succ(B,A).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_len15(A,B):-length(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_len17(A,B):-length(A,B).
my_reverse18(A,B):-reverse(A,B).
my_tail19([_|TL],TL).
my_last20(A,B):-last(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_succ22(A,B):-succ(A,B).
my_reverse23(A,B):-reverse(A,B).
prim(my_tail0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_max_list2,[list(int),int]).
prim(my_min_list3,[list(int),int]).
prim(my_pred4,[int,int]).
prim(my_pred5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_head8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_len10,[list(T),int]).
prim(my_len11,[list(T),int]).
prim(my_head12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_tail14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_sumlist16,[list(int),int]).
prim(my_len17,[list(T),int]).
prim(my_reverse18,[list(T),T]).
prim(my_tail19,[list(T),T]).
prim(my_last20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_succ22,[int,int]).
prim(my_reverse23,[list(T),T]).
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
p([['q','t','t','r'],['n','e','d','d'],['c','k','a'],['u','f','e','h']],[['q','t','t'],['n','e','d'],['c','k'],['u','f','e']]).
p([['g','y','d'],['o','l','h','i']],[['g','y'],['o','l','h']]).