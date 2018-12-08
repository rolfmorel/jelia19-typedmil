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

my_lowercase3(A):-downcase_atom(A,A).
my_even4(A):-0 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_even4/1).
prim(my_min_list5/2).
prim(my_tolower6/2).
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
p([['M','W','X'],['M','c','a']],[['M','W'],['M','c']]).
p([['g','V','o','t'],['X','p','w','L'],['q','m','F','n']],[['g','V','o'],['X','p','w'],['q','m','F']]).
p([['P','s','t','u'],['u','z','u']],[['P','s','t'],['u','z']]).
p([['n','D','p'],['y','X','P'],['j','J','Q','d']],[['n','D'],['y','X'],['j','J','Q']]).
p([['u','E','H'],['g','g','k','Q'],['J','i','R','P'],['P','e','j','d']],[['u','E'],['g','g','k'],['J','i','R'],['P','e','j']]).
q([['T','j','y'],['d','n','u','y'],['o','t','F']],[['T','j','y'],['d','n','u','y'],['o','t']]).
q([['Y','u','V','P'],['I','s','a','t'],['V','F','M'],['C','Q','O','V']],[['Y','u','V','P'],['I','s','a','t'],['V','F'],['C','Q','O','V']]).
q([['b','u','h','N'],['U','m','V']],[['b','u','h','N'],['U','m']]).
q([['T','j','U','p'],['O','y','i'],['A','w','M','u'],['H','W','f']],[['T','j','U','p'],['O','y'],['A','w','M','u'],['H','W']]).
q([['p','T','G','g'],['N','V','J'],['N','M','q','o'],['U','d','m']],[['p','T','G','g'],['N','V'],['N','M','q'],['U','d','m']]).
