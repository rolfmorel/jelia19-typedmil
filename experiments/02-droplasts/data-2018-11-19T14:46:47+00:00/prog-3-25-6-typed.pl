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
my_reverse1(A,B):-reverse(A,B).
my_max_list2(A,B):-max_list(A,B).
my_tail3([_|TL],TL).
my_reverse4(A,B):-reverse(A,B).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B).
my_reverse9(A,B):-reverse(A,B).
my_reverse10(A,B):-reverse(A,B).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_tail13([_|TL],TL).
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_max_list16(A,B):-max_list(A,B).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_len19(A,B):-length(A,B).
my_last20(A,B):-last(A,B).
my_succ21(A,B):-succ(A,B).
my_pred22(A,B):-succ(B,A).
my_pred23(A,B):-succ(B,A).
my_head24([H|_],H).
prim(my_max_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_max_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_reverse4,[list(T),T]).
prim(my_reverse5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_reverse9,[list(T),T]).
prim(my_reverse10,[list(T),T]).
prim(my_reverse11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_tail13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_max_list16,[list(int),int]).
prim(my_tail17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_len19,[list(T),int]).
prim(my_last20,[list(T),T]).
prim(my_succ21,[int,int]).
prim(my_pred22,[int,int]).
prim(my_pred23,[int,int]).
prim(my_head24,[list(T),T]).
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
p([['l','v','j'],['t','h','d'],['o','w','f']],[['l','v'],['t','h'],['o','w']]).
p([['a','i','l'],['t','c','f','h'],['s','o','i','t'],['o','p','t','j']],[['a','i'],['t','c','f'],['s','o','i'],['o','p','t']]).
