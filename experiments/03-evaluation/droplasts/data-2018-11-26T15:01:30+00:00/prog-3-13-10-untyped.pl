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

my_succ3(A,B):-succ(A,B),B =< 10.
my_toupper4(A,B):-upcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_flatten6(A,B):-flatten(A,B).
my_len7(A,B):-length(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_even10(A):-0 is A mod 2.
my_sumlist11(A,B):-sumlist(A,B).
my_tolower12(A,B):-downcase_atom(A,B).
my_set13(A):-list_to_set(A,A).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_toupper4/2).
prim(my_lowercase5/1).
prim(my_flatten6/2).
prim(my_len7/2).
prim(my_uppercase8/1).
prim(my_pred9/2).
prim(my_even10/1).
prim(my_sumlist11/2).
prim(my_tolower12/2).
prim(my_set13/1).
prim(my_min_list14/2).
prim(my_head15/2).
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
p([['m','J','O','x'],['k','d','N'],['c','z','E']],[['m','J','O'],['k','d'],['c','z']]).
p([['u','o','f'],['u','G','u','E'],['i','G','d']],[['u','o'],['u','G','u'],['i','G']]).
p([['h','L','v','o'],['R','M','G'],['c','k','q']],[['h','L','v'],['R','M'],['c','k']]).
p([['y','Q','D','D'],['q','H','Z']],[['y','Q','D'],['q','H']]).
p([['S','s','k'],['j','D','Q','e']],[['S','s'],['j','D','Q']]).
q([['E','Y','A','h'],['e','l','l']],[['E','Y','A','h'],['e','l']]).
q([['j','B','Z'],['u','G','a','Y'],['v','O','W'],['T','w','d','V']],[['j','B','Z'],['u','G','a'],['v','O','W'],['T','w','d']]).
q([['G','C','j','E'],['y','H','z','f'],['W','C','v'],['B','q','O','N']],[['G','C','j'],['y','H','z','f'],['W','C','v'],['B','q','O']]).
q([['a','v','M','S'],['o','A','P','p'],['U','O','p']],[['a','v','M'],['o','A','P','p'],['U','O','p']]).
q([['K','K','C','t'],['k','k','v','M'],['J','v','K','t'],['r','P','f']],[['K','K','C','t'],['k','k','v'],['J','v','K','t'],['r','P','f']]).
