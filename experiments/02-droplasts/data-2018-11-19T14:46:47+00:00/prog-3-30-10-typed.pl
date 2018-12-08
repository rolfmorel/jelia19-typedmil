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
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_max_list8(A,B):-max_list(A,B).
my_min_list9(A,B):-min_list(A,B).
my_head10([H|_],H).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A).
my_succ13(A,B):-succ(A,B).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_succ18(A,B):-succ(A,B).
my_head19([H|_],H).
my_head20([H|_],H).
my_head21([H|_],H).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_succ24(A,B):-succ(A,B).
my_pred25(A,B):-succ(B,A).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
my_last28(A,B):-last(A,B).
my_len29(A,B):-length(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_min_list3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_len5,[list(T),int]).
prim(my_last6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_max_list8,[list(int),int]).
prim(my_min_list9,[list(int),int]).
prim(my_head10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_tail14,[list(T),T]).
prim(my_tail15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_succ18,[int,int]).
prim(my_head19,[list(T),T]).
prim(my_head20,[list(T),T]).
prim(my_head21,[list(T),T]).
prim(my_len22,[list(T),int]).
prim(my_last23,[list(T),T]).
prim(my_succ24,[int,int]).
prim(my_pred25,[int,int]).
prim(my_head26,[list(T),T]).
prim(my_last27,[list(T),T]).
prim(my_last28,[list(T),T]).
prim(my_len29,[list(T),int]).
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
p([['t','r','k','k'],['f','n','t'],['g','u','g'],['t','u','g']],[['t','r','k'],['f','n'],['g','u'],['t','u']]).
p([['t','j','r','v'],['a','u','q'],['i','q','b','s']],[['t','j','r'],['a','u'],['i','q','b']]).
