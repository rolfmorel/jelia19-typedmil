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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_pred4(A,B):-succ(B,A).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_len9(A,B):-length(A,B).
my_pred10(A,B):-succ(B,A).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_len16(A,B):-length(A,B).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_pred19(A,B):-succ(B,A).
prim(my_max_list0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_min_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_tail6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_len8,[list(T),int]).
prim(my_len9,[list(T),int]).
prim(my_pred10,[int,int]).
prim(my_sumlist11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_pred13,[int,int]).
prim(my_pred14,[int,int]).
prim(my_reverse15,[list(T),T]).
prim(my_len16,[list(T),int]).
prim(my_reverse17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_pred19,[int,int]).
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
p([['g','u','o','p'],['n','v','a','y'],['o','r','b','w']],[['g','u','o'],['n','v','a'],['o','r','b']]).
p([['y','e','g'],['o','h','b'],['a','f','n','g']],[['y','e'],['o','h'],['a','f','n']]).
