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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_sumlist4(A,B):-sumlist(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_len7(A,B):-length(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_min_list16(A,B):-min_list(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_tail1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_sumlist4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_min_list6,[list(int),int]).
prim(my_len7,[list(T),int]).
prim(my_last8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_len11,[list(T),int]).
prim(my_min_list12,[list(int),int]).
prim(my_sumlist13,[list(int),int]).
prim(my_max_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_min_list16,[list(int),int]).
prim(my_tail17,[list(T),T]).
prim(my_len18,[list(T),int]).
prim(my_head19,[list(T),T]).
prim(my_pred20,[int,int]).
prim(my_sumlist21,[list(int),int]).
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
p([['s','u','m','y'],['a','u','r','n'],['v','s','c','x'],['x','m','k']],[['s','u','m'],['a','u','r'],['v','s','c'],['x','m']]).
p([['d','v','t','e'],['f','t','w']],[['d','v','t'],['f','t']]).
