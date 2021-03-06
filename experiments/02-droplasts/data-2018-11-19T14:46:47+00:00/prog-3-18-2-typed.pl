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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
my_len15(A,B):-length(A,B).
my_tail16([_|TL],TL).
my_reverse17(A,B):-reverse(A,B).
prim(my_last0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_reverse5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_len8,[list(T),int]).
prim(my_max_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_len11,[list(T),int]).
prim(my_max_list12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_tail14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_tail16,[list(T),T]).
prim(my_reverse17,[list(T),T]).
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
p([['x','q','c','q'],['j','r','u','o'],['d','r','f'],['w','n','h','i']],[['x','q','c'],['j','r','u'],['d','r'],['w','n','h']]).
p([['h','v','m','j'],['e','e','v']],[['h','v','m'],['e','e']]).
