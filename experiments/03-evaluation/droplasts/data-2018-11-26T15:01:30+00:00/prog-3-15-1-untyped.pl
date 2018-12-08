:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_msort3(A,B):-msort(A,B).
my_last4(A,B):-last(A,B).
my_set5(A):-list_to_set(A,A).
my_lowercase6(A):-downcase_atom(A,A).
my_double7(N,M):-M is 2*N,M =< 10.
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_min_list10(A,B):-min_list(A,B).
my_head11([H|_],H).
my_pred12(A,B):-succ(B,A),A > 0.
my_len13(A,B):-length(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_flatten15(A,B):-flatten(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_odd17(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_last4/2).
prim(my_set5/1).
prim(my_lowercase6/1).
prim(my_double7/2).
prim(my_list_to_set8/2).
prim(my_succ9/2).
prim(my_min_list10/2).
prim(my_head11/2).
prim(my_pred12/2).
prim(my_len13/2).
prim(my_tolower14/2).
prim(my_flatten15/2).
prim(my_sumlist16/2).
prim(my_odd17/1).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['R','i','b','b'],['P','o','G']],[['R','i','b'],['P','o']]).
p([['A','m','U','c'],['M','k','v']],[['A','m','U'],['M','k']]).
p([['C','Q','Z'],['j','q','I']],[['C','Q'],['j','q']]).
p([['w','D','U'],['b','P','K'],['O','O','u'],['b','I','c','d']],[['w','D'],['b','P'],['O','O'],['b','I','c']]).
p([['l','Y','A','h'],['g','Q','R','A'],['J','F','E'],['O','q','m']],[['l','Y','A'],['g','Q','R'],['J','F'],['O','q']]).
q([['P','x','p','T'],['s','L','t','W']],[['P','x','p'],['s','L','t','W']]).
q([['K','Q','I'],['k','N','L','m']],[['K','Q','I'],['k','N','L']]).
q([['j','i','u','X'],['O','K','t']],[['j','i','u'],['O','K','t']]).
q([['y','N','V'],['r','x','X']],[['y','N'],['r','x','X']]).
q([['S','a','Y'],['o','l','P','I'],['Q','X','N','c'],['o','m','u']],[['S','a','Y'],['o','l','P'],['Q','X','N'],['o','m','u']]).
