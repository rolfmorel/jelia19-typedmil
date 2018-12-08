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

my_element3(A,B):-member(B,A).
my_len4(A,B):-length(A,B).
my_set5(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_element3/2).
prim(my_len4/2).
prim(my_set5/1).
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
p([['X','f','q','U'],['k','k','u']],[['X','f','q'],['k','k']]).
p([['N','B','S'],['z','m','c','h'],['e','x','J']],[['N','B'],['z','m','c'],['e','x']]).
p([['E','s','y','Q'],['C','b','v'],['J','P','N','S']],[['E','s','y'],['C','b'],['J','P','N']]).
p([['K','G','g'],['G','c','n','W']],[['K','G'],['G','c','n']]).
p([['o','S','M'],['E','L','g']],[['o','S'],['E','L']]).
q([['c','S','P','Y'],['J','g','F','G'],['d','O','m','C']],[['c','S','P','Y'],['J','g','F'],['d','O','m','C']]).
q([['K','h','w','Z'],['G','T','q'],['M','p','M','P'],['n','J','d']],[['K','h','w','Z'],['G','T','q'],['M','p','M'],['n','J','d']]).
q([['j','W','B','w'],['S','x','n'],['v','l','Z','n']],[['j','W','B','w'],['S','x','n'],['v','l','Z']]).
q([['A','w','y','C'],['S','g','u','X']],[['A','w','y','C'],['S','g','u']]).
q([['n','Q','O','W'],['l','d','C']],[['n','Q','O','W'],['l','d']]).
