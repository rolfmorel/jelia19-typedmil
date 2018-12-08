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

my_flatten3(A,B):-flatten(A,B).
my_element4(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_element4,[list(T),T]).
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
p([['c','l','H'],['p','C','v']],[['c','l'],['p','C']]).
p([['E','O','B','w'],['A','A','q','y']],[['E','O','B'],['A','A','q']]).
p([['F','g','A','j'],['n','G','w','p'],['q','D','D','q']],[['F','g','A'],['n','G','w'],['q','D','D']]).
p([['b','N','l'],['M','c','H']],[['b','N'],['M','c']]).
p([['R','U','s'],['p','k','q'],['Z','M','h']],[['R','U'],['p','k'],['Z','M']]).
q([['Z','V','I'],['s','U','D','m']],[['Z','V'],['s','U','D','m']]).
q([['G','H','w','t'],['Y','f','x','J'],['V','N','I','p'],['g','V','n']],[['G','H','w'],['Y','f','x','J'],['V','N','I'],['g','V','n']]).
q([['t','y','n','p'],['N','D','a','o'],['a','t','T','l'],['f','r','t']],[['t','y','n','p'],['N','D','a','o'],['a','t','T'],['f','r','t']]).
q([['T','H','L'],['i','E','p','u'],['D','a','R','l'],['w','t','N','n']],[['T','H'],['i','E','p','u'],['D','a','R'],['w','t','N','n']]).
q([['x','x','k'],['X','B','K','c']],[['x','x','k'],['X','B','K']]).
