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

my_len3(A,B):-length(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_max_list5(A,B):-max_list(A,B).
my_msort6(A,B):-msort(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_double8(N,M):-M is 2*N,M =< 10.
my_min_list9(A,B):-min_list(A,B).
my_tolower10(A,B):-downcase_atom(A,B).
my_set11(A):-list_to_set(A,A).
my_even12(A):-0 is A mod 2.
my_uppercase13(A):-upcase_atom(A,A).
my_succ14(A,B):-succ(A,B),B =< 10.
my_lowercase15(A):-downcase_atom(A,A).
my_flatten16(A,B):-flatten(A,B).
my_odd17(A):-1 is A mod 2.
my_head18([H|_],H).
my_sumlist19(A,B):-sumlist(A,B).
my_element20(A,B):-member(B,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_len3/2).
prim(my_list_to_set4/2).
prim(my_max_list5/2).
prim(my_msort6/2).
prim(my_double8/2).
prim(my_min_list9/2).
prim(my_tolower10/2).
prim(my_set11/1).
prim(my_even12/1).
prim(my_uppercase13/1).
prim(my_succ14/2).
prim(my_lowercase15/1).
prim(my_flatten16/2).
prim(my_odd17/1).
prim(my_head18/2).
prim(my_sumlist19/2).
prim(my_element20/2).
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
p([['r','K','r','L'],['s','g','y','M']],[['r','K','r'],['s','g','y']]).
p([['Y','p','Q','t'],['j','S','k','I'],['t','R','k'],['u','h','E','v']],[['Y','p','Q'],['j','S','k'],['t','R'],['u','h','E']]).
p([['o','C','P','S'],['N','M','w'],['T','f','B'],['e','y','C','O']],[['o','C','P'],['N','M'],['T','f'],['e','y','C']]).
p([['s','N','y','Q'],['E','J','j'],['M','l','J'],['a','s','B','t']],[['s','N','y'],['E','J'],['M','l'],['a','s','B']]).
p([['U','C','N','a'],['u','R','H','t']],[['U','C','N'],['u','R','H']]).
q([['O','y','B'],['K','o','H','Y']],[['O','y'],['K','o','H','Y']]).
q([['e','f','d'],['v','V','a'],['w','U','z']],[['e','f','d'],['v','V','a'],['w','U']]).
q([['W','K','B','p'],['T','C','o']],[['W','K','B','p'],['T','C']]).
q([['e','z','c','k'],['S','o','Z']],[['e','z','c'],['S','o','Z']]).
q([['H','K','o','R'],['H','d','S'],['d','M','q','M'],['Z','N','E']],[['H','K','o','R'],['H','d'],['d','M','q','M'],['Z','N']]).
