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

my_tolower3(A,B):-downcase_atom(A,B).
my_odd4(A):-1 is A mod 2.
my_head5([H|_],H).
my_max_list6(A,B):-max_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_flatten8(A,B):-flatten(A,B).
my_set9(A):-list_to_set(A,A).
my_len10(A,B):-length(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_toupper12(A,B):-upcase_atom(A,B).
my_last13(A,B):-last(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_lowercase15(A):-downcase_atom(A,A).
my_list_to_set16(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_tolower3/2).
prim(my_odd4/1).
prim(my_head5/2).
prim(my_max_list6/2).
prim(my_pred7/2).
prim(my_flatten8/2).
prim(my_set9/1).
prim(my_len10/2).
prim(my_uppercase11/1).
prim(my_toupper12/2).
prim(my_last13/2).
prim(my_succ14/2).
prim(my_lowercase15/1).
prim(my_list_to_set16/2).
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
p([['h','h','N','Z'],['K','d','L','z'],['F','b','D','i']],[['h','h','N'],['K','d','L'],['F','b','D']]).
p([['P','i','E','w'],['O','i','b'],['Y','C','t','p']],[['P','i','E'],['O','i'],['Y','C','t']]).
p([['S','M','H','X'],['B','w','b']],[['S','M','H'],['B','w']]).
p([['V','l','t'],['z','H','W','E']],[['V','l'],['z','H','W']]).
p([['o','w','Y','h'],['w','u','I','G'],['P','R','U']],[['o','w','Y'],['w','u','I'],['P','R']]).
q([['N','z','D','p'],['L','g','p']],[['N','z','D','p'],['L','g']]).
q([['f','H','d'],['W','V','H','v'],['T','j','p'],['A','A','A']],[['f','H','d'],['W','V','H'],['T','j','p'],['A','A']]).
q([['N','T','p','B'],['c','E','v','R'],['e','w','z','B'],['e','D','A','X']],[['N','T','p','B'],['c','E','v','R'],['e','w','z'],['e','D','A','X']]).
q([['N','d','B'],['e','I','Q','A'],['x','c','M'],['V','T','c','n']],[['N','d','B'],['e','I','Q'],['x','c','M'],['V','T','c','n']]).
q([['C','Y','j'],['f','X','c','U']],[['C','Y'],['f','X','c','U']]).
