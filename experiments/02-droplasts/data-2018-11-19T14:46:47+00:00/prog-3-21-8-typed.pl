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
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_head3([H|_],H).
my_tail4([_|TL],TL).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_max_list10(A,B):-max_list(A,B).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_tail14([_|TL],TL).
my_min_list15(A,B):-min_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_succ17(A,B):-succ(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_reverse20(A,B):-reverse(A,B).
prim(my_last0,[list(T),T]).
prim(my_max_list1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_head3,[list(T),T]).
prim(my_tail4,[list(T),T]).
prim(my_head5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_len7,[list(T),int]).
prim(my_reverse8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_max_list10,[list(int),int]).
prim(my_tail11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_tail14,[list(T),T]).
prim(my_min_list15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_succ17,[int,int]).
prim(my_sumlist18,[list(int),int]).
prim(my_reverse19,[list(T),T]).
prim(my_reverse20,[list(T),T]).
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
p([['k','p','v'],['a','l','n'],['j','f','t','n']],[['k','p'],['a','l'],['j','f','t']]).
p([['y','x','q','i'],['u','q','g'],['k','c','c','n']],[['y','x','q'],['u','q'],['k','c','c']]).
