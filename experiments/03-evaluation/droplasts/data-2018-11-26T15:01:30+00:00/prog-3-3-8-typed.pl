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

my_set3(A):-list_to_set(A,A).
my_odd4(A):-1 is A mod 2.
my_succ5(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_odd4,[int]).
prim(my_succ5,[int,int]).
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
p([['G','x','n','E'],['j','G','i'],['j','B','v','R']],[['G','x','n'],['j','G'],['j','B','v']]).
p([['r','U','a'],['L','s','U'],['o','X','x']],[['r','U'],['L','s'],['o','X']]).
p([['p','D','u','T'],['p','h','H']],[['p','D','u'],['p','h']]).
p([['C','E','T'],['f','v','P','D']],[['C','E'],['f','v','P']]).
p([['o','q','M','b'],['C','l','f','R'],['N','O','t'],['o','n','Z','o']],[['o','q','M'],['C','l','f'],['N','O'],['o','n','Z']]).
q([['Q','Y','N'],['E','e','y'],['n','q','L'],['A','r','q']],[['Q','Y'],['E','e','y'],['n','q','L'],['A','r','q']]).
q([['K','i','q'],['r','O','v','w']],[['K','i'],['r','O','v','w']]).
q([['g','w','y'],['n','u','r','n']],[['g','w'],['n','u','r','n']]).
q([['v','Z','u'],['S','Z','U']],[['v','Z'],['S','Z','U']]).
q([['h','l','r'],['p','C','C'],['k','J','J','k']],[['h','l','r'],['p','C','C'],['k','J','J']]).
