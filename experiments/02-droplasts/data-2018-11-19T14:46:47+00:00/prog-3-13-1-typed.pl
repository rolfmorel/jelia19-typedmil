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
my_head0([H|_],H).
my_pred1(A,B):-succ(B,A).
my_pred2(A,B):-succ(B,A).
my_len3(A,B):-length(A,B).
my_head4([H|_],H).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_reverse8(A,B):-reverse(A,B).
my_tail9([_|TL],TL).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_len12(A,B):-length(A,B).
prim(my_head0,[list(T),T]).
prim(my_pred1,[int,int]).
prim(my_pred2,[int,int]).
prim(my_len3,[list(T),int]).
prim(my_head4,[list(T),T]).
prim(my_head5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_head7,[list(T),T]).
prim(my_reverse8,[list(T),T]).
prim(my_tail9,[list(T),T]).
prim(my_len10,[list(T),int]).
prim(my_reverse11,[list(T),T]).
prim(my_len12,[list(T),int]).
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
p([['q','n','m'],['o','j','j','h'],['j','k','p'],['f','d','c']],[['q','n'],['o','j','j'],['j','k'],['f','d']]).
p([['x','n','b'],['x','c','r'],['p','w','a']],[['x','n'],['x','c'],['p','w']]).
