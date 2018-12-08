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

my_even3(A):-0 is A mod 2.
my_double4(N,M):-M is 2*N,M =< 10.
my_last5(A,B):-last(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_tolower7(A,B):-downcase_atom(A,B).
my_msort8(A,B):-msort(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_toupper10(A,B):-upcase_atom(A,B).
my_head11([H|_],H).
my_flatten12(A,B):-flatten(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_element14(A,B):-member(B,A).
my_odd15(A):-1 is A mod 2.
my_sumlist16(A,B):-sumlist(A,B).
my_succ17(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_double4/2).
prim(my_last5/2).
prim(my_lowercase6/1).
prim(my_tolower7/2).
prim(my_msort8/2).
prim(my_pred9/2).
prim(my_toupper10/2).
prim(my_head11/2).
prim(my_flatten12/2).
prim(my_list_to_set13/2).
prim(my_element14/2).
prim(my_odd15/1).
prim(my_sumlist16/2).
prim(my_succ17/2).
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
p([['s','O','f'],['x','J','D','Z'],['Y','s','c','A']],[['s','O'],['x','J','D'],['Y','s','c']]).
p([['y','U','H'],['z','W','M'],['u','D','l','Y']],[['y','U'],['z','W'],['u','D','l']]).
p([['C','Q','b'],['I','I','Q','m'],['r','z','I'],['L','g','a']],[['C','Q'],['I','I','Q'],['r','z'],['L','g']]).
p([['U','N','Q'],['l','c','n'],['s','k','Q']],[['U','N'],['l','c'],['s','k']]).
p([['S','G','x','d'],['d','t','y']],[['S','G','x'],['d','t']]).
q([['u','u','h'],['u','v','l'],['s','b','d']],[['u','u','h'],['u','v'],['s','b','d']]).
q([['l','t','n','W'],['E','W','S']],[['l','t','n','W'],['E','W']]).
q([['n','k','q','r'],['v','Z','d'],['y','E','y','F']],[['n','k','q','r'],['v','Z','d'],['y','E','y']]).
q([['P','H','e'],['L','k','m','j'],['f','T','B','i']],[['P','H'],['L','k','m','j'],['f','T','B','i']]).
q([['o','K','T','B'],['b','d','K'],['h','J','F'],['N','E','k']],[['o','K','T'],['b','d','K'],['h','J','F'],['N','E','k']]).
