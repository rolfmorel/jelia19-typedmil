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
my_reverse1(A,B):-reverse(A,B).
my_pred2(A,B):-succ(B,A).
my_reverse3(A,B):-reverse(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_tail13([_|TL],TL).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_head20([H|_],H).
prim(my_tail0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_reverse3,[list(T),T]).
prim(my_sumlist4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_max_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_tail9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_max_list11,[list(int),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_tail13,[list(T),T]).
prim(my_head14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_head17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_sumlist19,[list(int),int]).
prim(my_head20,[list(T),T]).
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
p([['f','i','i','d'],['w','r','u'],['c','e','e']],[['f','i','i'],['w','r'],['c','e']]).
p([['p','u','a','q'],['b','d','d','i'],['w','g','p'],['j','n','o','h']],[['p','u','a'],['b','d','d'],['w','g'],['j','n','o']]).
