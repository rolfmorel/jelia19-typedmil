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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_sumlist6(A,B):-sumlist(A,B).
my_min_list7(A,B):-min_list(A,B).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_len10(A,B):-length(A,B).
my_pred11(A,B):-succ(B,A).
my_reverse12(A,B):-reverse(A,B).
my_head13([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_tail1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_head3,[list(T),T]).
prim(my_head4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_min_list7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_len10,[list(T),int]).
prim(my_pred11,[int,int]).
prim(my_reverse12,[list(T),T]).
prim(my_head13,[list(T),T]).
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
p([['i','p','i','a'],['h','c','d','o'],['j','b','r','c']],[['i','p','i'],['h','c','d'],['j','b','r']]).
p([['a','i','o','q'],['w','a','f','t'],['x','d','f']],[['a','i','o'],['w','a','f'],['x','d']]).
