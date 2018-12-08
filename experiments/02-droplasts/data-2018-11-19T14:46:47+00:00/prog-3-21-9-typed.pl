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
my_max_list1(A,B):-max_list(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_head5([H|_],H).
my_max_list6(A,B):-max_list(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_head13([H|_],H).
my_head14([H|_],H).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
prim(my_len0,[list(T),int]).
prim(my_max_list1,[list(int),int]).
prim(my_sumlist2,[list(int),int]).
prim(my_min_list3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_head5,[list(T),T]).
prim(my_max_list6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_len10,[list(T),int]).
prim(my_min_list11,[list(int),int]).
prim(my_max_list12,[list(int),int]).
prim(my_head13,[list(T),T]).
prim(my_head14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_len19,[list(T),int]).
prim(my_pred20,[int,int]).
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
p([['q','g','w','y'],['m','x','s'],['u','k','x','g'],['l','t','r','q']],[['q','g','w'],['m','x'],['u','k','x'],['l','t','r']]).
p([['c','h','r'],['n','m','c']],[['c','h'],['n','m']]).
