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
my_max_list0(A,B):-max_list(A,B).
my_max_list1(A,B):-max_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_max_list5(A,B):-max_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_pred11(A,B):-succ(B,A).
my_succ12(A,B):-succ(A,B).
my_len13(A,B):-length(A,B).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_tail16([_|TL],TL).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_max_list1,[list(int),int]).
prim(my_min_list2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_succ4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_sumlist9,[list(int),int]).
prim(my_sumlist10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_succ12,[int,int]).
prim(my_len13,[list(T),int]).
prim(my_max_list14,[list(int),int]).
prim(my_tail15,[list(T),T]).
prim(my_tail16,[list(T),T]).
prim(my_sumlist17,[list(int),int]).
prim(my_last18,[list(T),T]).
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
p([['d','d','x','h'],['x','b','x','n']],[['d','d','x'],['x','b','x']]).
p([['h','g','u'],['i','a','v'],['p','j','f','a'],['x','p','u']],[['h','g'],['i','a'],['p','j','f'],['x','p']]).
