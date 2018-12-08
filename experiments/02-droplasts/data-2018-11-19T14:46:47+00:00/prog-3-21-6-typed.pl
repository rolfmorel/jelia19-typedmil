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
my_sumlist1(A,B):-sumlist(A,B).
my_last2(A,B):-last(A,B).
my_reverse3(A,B):-reverse(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_reverse6(A,B):-reverse(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_succ11(A,B):-succ(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_sumlist14(A,B):-sumlist(A,B).
my_tail15([_|TL],TL).
my_len16(A,B):-length(A,B).
my_succ17(A,B):-succ(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_len20(A,B):-length(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_last2,[list(T),T]).
prim(my_reverse3,[list(T),T]).
prim(my_head4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_reverse6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_len9,[list(T),int]).
prim(my_len10,[list(T),int]).
prim(my_succ11,[int,int]).
prim(my_tail12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_tail15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_succ17,[int,int]).
prim(my_len18,[list(T),int]).
prim(my_pred19,[int,int]).
prim(my_len20,[list(T),int]).
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
p([['l','s','n','d'],['k','l','e','q'],['d','c','i','s']],[['l','s','n'],['k','l','e'],['d','c','i']]).
p([['k','w','v'],['d','c','r']],[['k','w'],['d','c']]).
