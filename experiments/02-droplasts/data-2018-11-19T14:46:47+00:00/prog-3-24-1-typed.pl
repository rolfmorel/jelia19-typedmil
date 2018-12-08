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
my_sumlist0(A,B):-sumlist(A,B).
my_reverse1(A,B):-reverse(A,B).
my_tail2([_|TL],TL).
my_len3(A,B):-length(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_max_list15(A,B):-max_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_head18([H|_],H).
my_max_list19(A,B):-max_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_last23(A,B):-last(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_tail2,[list(T),T]).
prim(my_len3,[list(T),int]).
prim(my_succ4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_reverse6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_tail8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_len10,[list(T),int]).
prim(my_max_list11,[list(int),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_min_list13,[list(int),int]).
prim(my_reverse14,[list(T),T]).
prim(my_max_list15,[list(int),int]).
prim(my_min_list16,[list(int),int]).
prim(my_min_list17,[list(int),int]).
prim(my_head18,[list(T),T]).
prim(my_max_list19,[list(int),int]).
prim(my_reverse20,[list(T),T]).
prim(my_sumlist21,[list(int),int]).
prim(my_sumlist22,[list(int),int]).
prim(my_last23,[list(T),T]).
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
p([['b','c','p','w'],['s','g','u'],['t','r','j','p']],[['b','c','p'],['s','g'],['t','r','j']]).
p([['c','q','l','t'],['e','c','e']],[['c','q','l'],['e','c']]).
