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
my_head1([H|_],H).
my_len2(A,B):-length(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_len2,[list(T),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_max_list9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_succ11,[int,int]).
prim(my_reverse12,[list(T),T]).
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
p([['g','k','o','t'],['a','t','r','h'],['a','x','h','c'],['d','l','t']],[['g','k','o'],['a','t','r'],['a','x','h'],['d','l']]).
p([['i','n','x'],['n','r','l'],['t','u','s','h']],[['i','n'],['n','r'],['t','u','s']]).
