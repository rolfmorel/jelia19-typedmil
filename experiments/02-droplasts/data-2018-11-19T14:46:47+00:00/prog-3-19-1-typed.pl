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
my_succ0(A,B):-succ(A,B).
my_len1(A,B):-length(A,B).
my_last2(A,B):-last(A,B).
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_head16([H|_],H).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
prim(my_succ0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_last2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_len6,[list(T),int]).
prim(my_succ7,[int,int]).
prim(my_pred8,[int,int]).
prim(my_max_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_max_list15,[list(int),int]).
prim(my_head16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
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
p([['y','a','k'],['a','n','x','v'],['x','y','o','g'],['m','q','k']],[['y','a'],['a','n','x'],['x','y','o'],['m','q']]).
p([['m','w','v','t'],['l','u','e','m']],[['m','w','v'],['l','u','e']]).
