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
my_last0(A,B):-last(A,B).
my_pred1(A,B):-succ(B,A).
my_len2(A,B):-length(A,B).
my_min_list3(A,B):-min_list(A,B).
my_len4(A,B):-length(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_head12([H|_],H).
my_tail13([_|TL],TL).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_last16(A,B):-last(A,B).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_pred19(A,B):-succ(B,A).
my_head20([H|_],H).
my_reverse21(A,B):-reverse(A,B).
my_len22(A,B):-length(A,B).
prim(my_last0,[list(T),T]).
prim(my_pred1,[int,int]).
prim(my_len2,[list(T),int]).
prim(my_min_list3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_succ7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_tail11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_head14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_last16,[list(T),T]).
prim(my_last17,[list(T),T]).
prim(my_succ18,[int,int]).
prim(my_pred19,[int,int]).
prim(my_head20,[list(T),T]).
prim(my_reverse21,[list(T),T]).
prim(my_len22,[list(T),int]).
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
p([['q','l','a'],['o','e','f','e']],[['q','l'],['o','e','f']]).
p([['p','b','k','g'],['s','g','n'],['i','n','g'],['b','p','v','v']],[['p','b','k'],['s','g'],['i','n'],['b','p','v']]).
