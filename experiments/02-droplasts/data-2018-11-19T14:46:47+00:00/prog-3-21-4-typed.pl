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
my_pred0(A,B):-succ(B,A).
my_head1([H|_],H).
my_sumlist2(A,B):-sumlist(A,B).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_succ11(A,B):-succ(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_succ19(A,B):-succ(A,B).
my_sumlist20(A,B):-sumlist(A,B).
prim(my_pred0,[int,int]).
prim(my_head1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_tail4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_len6,[list(T),int]).
prim(my_pred7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_sumlist10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_reverse13,[list(T),T]).
prim(my_succ14,[int,int]).
prim(my_sumlist15,[list(int),int]).
prim(my_min_list16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_max_list18,[list(int),int]).
prim(my_succ19,[int,int]).
prim(my_sumlist20,[list(int),int]).
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
p([['f','s','j'],['k','l','a','x'],['j','u','r','n']],[['f','s'],['k','l','a'],['j','u','r']]).
p([['t','w','m','n'],['d','o','u'],['c','o','i','h'],['g','u','s']],[['t','w','m'],['d','o'],['c','o','i'],['g','u']]).
