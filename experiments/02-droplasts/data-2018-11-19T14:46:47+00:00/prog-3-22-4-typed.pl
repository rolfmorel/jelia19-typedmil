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
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_pred11(A,B):-succ(B,A).
my_max_list12(A,B):-max_list(A,B).
my_tail13([_|TL],TL).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_head16([H|_],H).
my_succ17(A,B):-succ(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_min_list21(A,B):-min_list(A,B).
prim(my_last0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_min_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_max_list12,[list(int),int]).
prim(my_tail13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_max_list15,[list(int),int]).
prim(my_head16,[list(T),T]).
prim(my_succ17,[int,int]).
prim(my_sumlist18,[list(int),int]).
prim(my_reverse19,[list(T),T]).
prim(my_sumlist20,[list(int),int]).
prim(my_min_list21,[list(int),int]).
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
p([['v','b','i','w'],['e','e','n','t'],['c','b','f','n']],[['v','b','i'],['e','e','n'],['c','b','f']]).
p([['e','q','y'],['s','w','s','k'],['w','u','f','l'],['e','l','l']],[['e','q'],['s','w','s'],['w','u','f'],['e','l']]).
