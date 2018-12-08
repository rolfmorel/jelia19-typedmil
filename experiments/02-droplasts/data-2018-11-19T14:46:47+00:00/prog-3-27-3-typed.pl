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
my_reverse0(A,B):-reverse(A,B).
my_succ1(A,B):-succ(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A).
my_len8(A,B):-length(A,B).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_succ11(A,B):-succ(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_len16(A,B):-length(A,B).
my_len17(A,B):-length(A,B).
my_succ18(A,B):-succ(A,B).
my_succ19(A,B):-succ(A,B).
my_last20(A,B):-last(A,B).
my_pred21(A,B):-succ(B,A).
my_tail22([_|TL],TL).
my_min_list23(A,B):-min_list(A,B).
my_tail24([_|TL],TL).
my_reverse25(A,B):-reverse(A,B).
my_len26(A,B):-length(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_len8,[list(T),int]).
prim(my_head9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_succ11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_succ14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_len17,[list(T),int]).
prim(my_succ18,[int,int]).
prim(my_succ19,[int,int]).
prim(my_last20,[list(T),T]).
prim(my_pred21,[int,int]).
prim(my_tail22,[list(T),T]).
prim(my_min_list23,[list(int),int]).
prim(my_tail24,[list(T),T]).
prim(my_reverse25,[list(T),T]).
prim(my_len26,[list(T),int]).
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
p([['n','c','c'],['f','f','r'],['g','y','c']],[['n','c'],['f','f'],['g','y']]).
p([['w','f','s'],['p','q','m','r']],[['w','f'],['p','q','m']]).
