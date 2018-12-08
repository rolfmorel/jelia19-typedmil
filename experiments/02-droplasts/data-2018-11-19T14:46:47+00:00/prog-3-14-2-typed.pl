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
my_tail1([_|TL],TL).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
prim(my_max_list0,[list(int),int]).
prim(my_tail1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_tail5,[list(T),T]).
prim(my_head6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_min_list8,[list(int),int]).
prim(my_reverse9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
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
p([['c','p','w'],['e','x','t','r'],['h','n','o'],['j','f','y','m']],[['c','p'],['e','x','t'],['h','n'],['j','f','y']]).
p([['m','b','y'],['n','e','p','k'],['a','c','s','s'],['x','j','j','f']],[['m','b'],['n','e','p'],['a','c','s'],['x','j','j']]).
