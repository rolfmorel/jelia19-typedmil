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
my_len0(A,B):-length(A,B).
my_tail1([_|TL],TL).
my_head2([H|_],H).
my_tail3([_|TL],TL).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_tail9([_|TL],TL).
my_min_list10(A,B):-min_list(A,B).
my_len11(A,B):-length(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
prim(my_len0,[list(T),int]).
prim(my_tail1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_tail3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_tail5,[list(T),T]).
prim(my_head6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_tail9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_len11,[list(T),int]).
prim(my_sumlist12,[list(int),int]).
prim(my_reverse13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_last15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_sumlist17,[list(int),int]).
prim(my_len18,[list(T),int]).
prim(my_pred19,[int,int]).
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
p([['m','s','i','y'],['l','v','m'],['i','a','f','f']],[['m','s','i'],['l','v'],['i','a','f']]).
p([['u','e','c'],['o','h','s']],[['u','e'],['o','h']]).
