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

my_sumlist3(A,B):-sumlist(A,B).
my_toupper4(A,B):-upcase_atom(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_msort6(A,B):-msort(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_toupper4,[char,char]).
prim(my_double5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_tolower7,[char,char]).
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
p([['X','S','d'],['W','M','K','D'],['i','r','t','X']],[['X','S'],['W','M','K'],['i','r','t']]).
p([['f','m','j'],['C','W','A','i']],[['f','m'],['C','W','A']]).
p([['z','n','R'],['u','u','D']],[['z','n'],['u','u']]).
p([['a','j','m','U'],['p','S','O'],['g','B','F','O']],[['a','j','m'],['p','S'],['g','B','F']]).
p([['q','E','c','f'],['l','h','W'],['t','k','x','q'],['s','L','M','W']],[['q','E','c'],['l','h'],['t','k','x'],['s','L','M']]).
q([['F','Y','q'],['S','s','h','E']],[['F','Y'],['S','s','h','E']]).
q([['Z','r','p'],['w','C','C'],['F','i','D'],['M','D','I','f']],[['Z','r','p'],['w','C'],['F','i'],['M','D','I','f']]).
q([['N','S','w','M'],['J','o','O','G']],[['N','S','w'],['J','o','O','G']]).
q([['L','x','x'],['r','t','c']],[['L','x'],['r','t','c']]).
q([['Y','h','A','N'],['K','x','H','X'],['W','j','c'],['k','o','s']],[['Y','h','A'],['K','x','H','X'],['W','j','c'],['k','o','s']]).
