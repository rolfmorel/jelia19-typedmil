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

my_uppercase3(A):-upcase_atom(A,A).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_odd6(A):-1 is A mod 2.
my_lowercase7(A):-downcase_atom(A,A).
my_list_to_set8(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_head4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_lowercase7,[char]).
prim(my_list_to_set8,[list(T),list(T)]).
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
p([['a','W','P','r'],['i','o','X']],[['a','W','P'],['i','o']]).
p([['Z','L','E','F'],['y','g','l','l'],['C','L','d','j']],[['Z','L','E'],['y','g','l'],['C','L','d']]).
p([['z','I','B'],['U','d','D'],['z','L','d']],[['z','I'],['U','d'],['z','L']]).
p([['h','o','q'],['T','g','u','R'],['q','m','w','T']],[['h','o'],['T','g','u'],['q','m','w']]).
p([['Z','K','k'],['Z','v','a','X'],['c','I','A','F'],['N','m','I','Q']],[['Z','K'],['Z','v','a'],['c','I','A'],['N','m','I']]).
q([['D','l','O'],['C','U','S','U'],['p','d','w'],['o','f','C','q']],[['D','l'],['C','U','S','U'],['p','d'],['o','f','C','q']]).
q([['v','G','H','g'],['U','q','o']],[['v','G','H','g'],['U','q']]).
q([['d','E','s'],['A','W','X'],['z','d','r']],[['d','E','s'],['A','W'],['z','d','r']]).
q([['Y','V','E','H'],['O','x','b'],['T','B','K','K'],['l','J','o']],[['Y','V','E'],['O','x'],['T','B','K','K'],['l','J','o']]).
q([['N','n','e'],['w','m','n'],['Y','L','g','U'],['i','g','U']],[['N','n'],['w','m','n'],['Y','L','g','U'],['i','g','U']]).
