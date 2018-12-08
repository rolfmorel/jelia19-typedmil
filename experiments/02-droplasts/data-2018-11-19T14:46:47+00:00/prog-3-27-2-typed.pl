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
my_succ0(A,B):-succ(A,B).
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_pred3(A,B):-succ(B,A).
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_reverse10(A,B):-reverse(A,B).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_min_list13(A,B):-min_list(A,B).
my_head14([H|_],H).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_max_list17(A,B):-max_list(A,B).
my_reverse18(A,B):-reverse(A,B).
my_last19(A,B):-last(A,B).
my_head20([H|_],H).
my_succ21(A,B):-succ(A,B).
my_tail22([_|TL],TL).
my_len23(A,B):-length(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_succ25(A,B):-succ(A,B).
my_reverse26(A,B):-reverse(A,B).
prim(my_succ0,[int,int]).
prim(my_head1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_pred3,[int,int]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_pred7,[int,int]).
prim(my_reverse8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_reverse10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_min_list13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_max_list17,[list(int),int]).
prim(my_reverse18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_head20,[list(T),T]).
prim(my_succ21,[int,int]).
prim(my_tail22,[list(T),T]).
prim(my_len23,[list(T),int]).
prim(my_sumlist24,[list(int),int]).
prim(my_succ25,[int,int]).
prim(my_reverse26,[list(T),T]).
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
p([['w','r','x'],['p','v','h','b'],['r','w','n','q']],[['w','r'],['p','v','h'],['r','w','n']]).
p([['t','u','g'],['d','e','c','r'],['i','m','c','w'],['o','d','h']],[['t','u'],['d','e','c'],['i','m','c'],['o','d']]).
