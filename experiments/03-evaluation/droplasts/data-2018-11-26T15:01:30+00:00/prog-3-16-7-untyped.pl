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
my_even4(A):-0 is A mod 2.
my_toupper5(A,B):-upcase_atom(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_uppercase8(A):-upcase_atom(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
my_max_list11(A,B):-max_list(A,B).
my_last12(A,B):-last(A,B).
my_lowercase13(A):-downcase_atom(A,A).
my_double14(N,M):-M is 2*N,M =< 10.
my_min_list15(A,B):-min_list(A,B).
my_element16(A,B):-member(B,A).
my_odd17(A):-1 is A mod 2.
my_sumlist18(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_even4/1).
prim(my_toupper5/2).
prim(my_tolower6/2).
prim(my_succ7/2).
prim(my_uppercase8/1).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_max_list11/2).
prim(my_last12/2).
prim(my_lowercase13/1).
prim(my_double14/2).
prim(my_min_list15/2).
prim(my_element16/2).
prim(my_odd17/1).
prim(my_sumlist18/2).
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
p([['J','Z','q','k'],['o','Z','s'],['n','w','m']],[['J','Z','q'],['o','Z'],['n','w']]).
p([['R','l','N','S'],['V','L','y'],['B','W','J','f']],[['R','l','N'],['V','L'],['B','W','J']]).
p([['P','A','k','K'],['r','i','W','K']],[['P','A','k'],['r','i','W']]).
p([['x','w','K','o'],['C','t','i'],['H','V','E','n']],[['x','w','K'],['C','t'],['H','V','E']]).
p([['O','E','w','R'],['f','P','T'],['P','J','U','V'],['G','A','q','z']],[['O','E','w'],['f','P'],['P','J','U'],['G','A','q']]).
q([['y','i','x','U'],['w','I','s']],[['y','i','x','U'],['w','I']]).
q([['F','Z','v'],['F','N','n'],['U','p','y','E'],['f','u','W']],[['F','Z'],['F','N','n'],['U','p','y'],['f','u','W']]).
q([['p','m','j','V'],['q','v','N','i'],['d','p','J','f'],['b','z','K']],[['p','m','j','V'],['q','v','N'],['d','p','J','f'],['b','z','K']]).
q([['Y','c','J','B'],['D','r','z']],[['Y','c','J'],['D','r','z']]).
q([['O','M','z','z'],['V','n','q'],['D','z','y','H'],['J','f','F','F']],[['O','M','z','z'],['V','n'],['D','z','y','H'],['J','f','F','F']]).
