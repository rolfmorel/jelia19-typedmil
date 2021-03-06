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

my_head3([H|_],H).
my_msort4(A,B):-msort(A,B).

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

my_len6(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_msort4/2).
prim(my_len6/2).
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
p([['p','j','t'],['F','Z','p','s'],['A','n','u','F'],['P','X','r']],[['p','j'],['F','Z','p'],['A','n','u'],['P','X']]).
p([['s','Y','B','g'],['N','j','Y'],['u','K','f','W']],[['s','Y','B'],['N','j'],['u','K','f']]).
p([['U','e','h','t'],['X','g','g'],['w','d','A']],[['U','e','h'],['X','g'],['w','d']]).
p([['l','O','i'],['o','v','x','l']],[['l','O'],['o','v','x']]).
p([['R','W','G'],['Q','A','u']],[['R','W'],['Q','A']]).
q([['f','C','Z'],['p','o','u'],['i','T','X','P'],['w','A','o','E']],[['f','C','Z'],['p','o'],['i','T','X','P'],['w','A','o','E']]).
q([['K','d','u'],['m','o','Q']],[['K','d','u'],['m','o']]).
q([['A','K','A'],['t','P','q','j'],['I','T','k'],['D','R','r','B']],[['A','K','A'],['t','P','q','j'],['I','T','k'],['D','R','r']]).
q([['i','l','L'],['V','Q','v'],['E','G','z','W']],[['i','l'],['V','Q','v'],['E','G','z','W']]).
q([['O','P','y','T'],['J','j','q'],['e','D','z']],[['O','P','y'],['J','j','q'],['e','D','z']]).
