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
my_reverse0(A,B):-reverse(A,B).
my_reverse1(A,B):-reverse(A,B).
my_last2(A,B):-last(A,B).
my_max_list3(A,B):-max_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_max_list8(A,B):-max_list(A,B).
my_tail9([_|TL],TL).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_max_list15(A,B):-max_list(A,B).
my_len16(A,B):-length(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_reverse4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_max_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_tail9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_sumlist14,[list(int),int]).
prim(my_max_list15,[list(int),int]).
prim(my_len16,[list(T),int]).
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
p([['j','f','y'],['p','b','o'],['x','h','e'],['j','d','i']],[['j','f'],['p','b'],['x','h'],['j','d']]).
p([['l','x','u','u'],['f','n','c','n'],['p','t','f','q']],[['l','x','u'],['f','n','c'],['p','t','f']]).
