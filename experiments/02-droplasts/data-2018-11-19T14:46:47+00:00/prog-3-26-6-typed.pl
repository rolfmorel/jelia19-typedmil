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
my_head0([H|_],H).
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_pred5(A,B):-succ(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_min_list14(A,B):-min_list(A,B).
my_last15(A,B):-last(A,B).
my_min_list16(A,B):-min_list(A,B).
my_len17(A,B):-length(A,B).
my_reverse18(A,B):-reverse(A,B).
my_reverse19(A,B):-reverse(A,B).
my_pred20(A,B):-succ(B,A).
my_tail21([_|TL],TL).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_head24([H|_],H).
my_pred25(A,B):-succ(B,A).
prim(my_head0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_max_list3,[list(int),int]).
prim(my_tail4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_sumlist6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_len10,[list(T),int]).
prim(my_len11,[list(T),int]).
prim(my_pred12,[int,int]).
prim(my_tail13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
prim(my_last15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_len17,[list(T),int]).
prim(my_reverse18,[list(T),T]).
prim(my_reverse19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_tail21,[list(T),T]).
prim(my_len22,[list(T),int]).
prim(my_last23,[list(T),T]).
prim(my_head24,[list(T),T]).
prim(my_pred25,[int,int]).
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
p([['f','a','k'],['a','a','t','s'],['v','i','r','d'],['e','b','w']],[['f','a'],['a','a','t'],['v','i','r'],['e','b']]).
p([['u','b','g'],['l','q','p'],['v','i','k'],['l','v','o','f']],[['u','b'],['l','q'],['v','i'],['l','v','o']]).
