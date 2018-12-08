:- use_module('metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail/2).
prim(reverse/2).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
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
prim(my_reverse0/2).
prim(my_reverse1/2).
prim(my_last2/2).
prim(my_max_list3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_max_list6/2).
prim(my_reverse7/2).
prim(my_max_list8/2).
prim(my_tail9/2).
prim(my_head10/2).
prim(my_tail11/2).
prim(my_max_list12/2).
prim(my_max_list13/2).
prim(my_sumlist14/2).
prim(my_max_list15/2).
prim(my_len16/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learn(Pos,[],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['j','f','y'],['p','b','o'],['x','h','e'],['j','d','i']],[['j','f'],['p','b'],['x','h'],['j','d']]).
p([['l','x','u','u'],['f','n','c','n'],['p','t','f','q']],[['l','x','u'],['f','n','c'],['p','t','f']]).
