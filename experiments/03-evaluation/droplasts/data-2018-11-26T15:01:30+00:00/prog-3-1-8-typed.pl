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

my_lowercase3(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_lowercase3,[char]).
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
p([['o','q','U','o'],['c','L','z'],['r','p','W','L'],['T','P','R','F']],[['o','q','U'],['c','L'],['r','p','W'],['T','P','R']]).
p([['k','q','W','W'],['b','K','L','n'],['k','B','K','a']],[['k','q','W'],['b','K','L'],['k','B','K']]).
p([['k','o','M'],['N','a','N'],['G','S','e']],[['k','o'],['N','a'],['G','S']]).
p([['w','P','r','o'],['U','L','L']],[['w','P','r'],['U','L']]).
p([['M','H','Y','z'],['g','F','w']],[['M','H','Y'],['g','F']]).
q([['K','N','f'],['L','n','R'],['g','E','I']],[['K','N','f'],['L','n'],['g','E','I']]).
q([['O','f','U','g'],['l','i','y','c']],[['O','f','U','g'],['l','i','y']]).
q([['w','z','x'],['l','Z','Y'],['x','o','p','j'],['g','x','k']],[['w','z'],['l','Z','Y'],['x','o','p','j'],['g','x','k']]).
q([['c','v','M'],['w','W','Y'],['E','F','V','k'],['B','V','t']],[['c','v','M'],['w','W','Y'],['E','F','V'],['B','V']]).
q([['v','c','D'],['q','n','W'],['D','b','H','z']],[['v','c','D'],['q','n'],['D','b','H','z']]).
