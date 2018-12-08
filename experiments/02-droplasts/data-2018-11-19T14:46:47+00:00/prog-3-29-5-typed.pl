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
my_reverse1(A,B):-reverse(A,B).
my_max_list2(A,B):-max_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_pred4(A,B):-succ(B,A).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_max_list7(A,B):-max_list(A,B).
my_pred8(A,B):-succ(B,A).
my_tail9([_|TL],TL).
my_max_list10(A,B):-max_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_tail16([_|TL],TL).
my_reverse17(A,B):-reverse(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_tail21([_|TL],TL).
my_head22([H|_],H).
my_last23(A,B):-last(A,B).
my_tail24([_|TL],TL).
my_tail25([_|TL],TL).
my_len26(A,B):-length(A,B).
my_pred27(A,B):-succ(B,A).
my_last28(A,B):-last(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_pred4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_tail6,[list(T),T]).
prim(my_max_list7,[list(int),int]).
prim(my_pred8,[int,int]).
prim(my_tail9,[list(T),T]).
prim(my_max_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_tail16,[list(T),T]).
prim(my_reverse17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_sumlist19,[list(int),int]).
prim(my_sumlist20,[list(int),int]).
prim(my_tail21,[list(T),T]).
prim(my_head22,[list(T),T]).
prim(my_last23,[list(T),T]).
prim(my_tail24,[list(T),T]).
prim(my_tail25,[list(T),T]).
prim(my_len26,[list(T),int]).
prim(my_pred27,[int,int]).
prim(my_last28,[list(T),T]).
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
p([['n','m','d','j'],['o','u','a'],['c','e','c'],['m','c','p','f']],[['n','m','d'],['o','u'],['c','e'],['m','c','p']]).
p([['e','f','g'],['a','x','c','p'],['e','y','f'],['j','t','j','e']],[['e','f'],['a','x','c'],['e','y'],['j','t','j']]).
