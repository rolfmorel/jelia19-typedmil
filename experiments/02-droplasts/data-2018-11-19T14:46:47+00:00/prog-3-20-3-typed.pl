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
my_pred1(A,B):-succ(B,A).
my_min_list2(A,B):-min_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_len0,[list(T),int]).
prim(my_pred1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_max_list3,[list(int),int]).
prim(my_min_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_max_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_len10,[list(T),int]).
prim(my_tail11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_head15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_head17,[list(T),T]).
prim(my_sumlist18,[list(int),int]).
prim(my_min_list19,[list(int),int]).
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
p([['v','l','y','p'],['o','t','p'],['l','a','l','x'],['h','i','g']],[['v','l','y'],['o','t'],['l','a','l'],['h','i']]).
p([['e','l','t'],['n','g','d','a']],[['e','l'],['n','g','d']]).
