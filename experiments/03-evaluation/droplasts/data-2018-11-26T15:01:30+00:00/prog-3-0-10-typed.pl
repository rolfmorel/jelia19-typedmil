:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['i','k','E'],['P','E','q','i']],[['i','k'],['P','E','q']]).
p([['L','i','q','t'],['y','B','W','o']],[['L','i','q'],['y','B','W']]).
p([['Z','W','V'],['c','q','l','V']],[['Z','W'],['c','q','l']]).
p([['a','G','y','d'],['n','C','k','W'],['Y','r','Y'],['N','V','M']],[['a','G','y'],['n','C','k'],['Y','r'],['N','V']]).
p([['t','y','j','W'],['z','h','t','S'],['f','s','I']],[['t','y','j'],['z','h','t'],['f','s']]).
q([['A','Z','b','j'],['T','Y','I','I']],[['A','Z','b','j'],['T','Y','I']]).
q([['M','A','T','r'],['B','r','p','N']],[['M','A','T','r'],['B','r','p']]).
q([['w','C','j'],['y','h','N'],['j','L','H','z']],[['w','C','j'],['y','h','N'],['j','L','H']]).
q([['e','G','f'],['s','k','r'],['P','X','d']],[['e','G','f'],['s','k'],['P','X','d']]).
q([['r','C','I','D'],['d','s','k'],['o','h','o']],[['r','C','I'],['d','s','k'],['o','h','o']]).
