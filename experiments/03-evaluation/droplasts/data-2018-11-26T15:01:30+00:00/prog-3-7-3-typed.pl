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
my_succ4(A,B):-succ(A,B),B =< 10.
my_flatten5(A,B):-flatten(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_toupper7(A,B):-upcase_atom(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_len9(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_succ4,[int,int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_lowercase6,[char]).
prim(my_toupper7,[char,char]).
prim(my_uppercase8,[char]).
prim(my_len9,[list(_),int]).
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
p([['z','W','N'],['y','U','v','G'],['f','P','H']],[['z','W'],['y','U','v'],['f','P']]).
p([['k','W','F'],['T','S','E','i']],[['k','W'],['T','S','E']]).
p([['u','i','B','U'],['P','I','d'],['H','k','G']],[['u','i','B'],['P','I'],['H','k']]).
p([['o','w','Z'],['n','P','H','V'],['w','Y','x']],[['o','w'],['n','P','H'],['w','Y']]).
p([['V','f','O','k'],['g','J','U'],['u','N','X'],['f','L','d']],[['V','f','O'],['g','J'],['u','N'],['f','L']]).
q([['X','Z','y'],['A','N','T','s'],['g','i','u']],[['X','Z','y'],['A','N','T'],['g','i','u']]).
q([['w','W','N'],['m','J','c']],[['w','W'],['m','J','c']]).
q([['Y','i','f'],['t','k','B'],['o','k','A','O'],['C','M','o','s']],[['Y','i'],['t','k','B'],['o','k','A'],['C','M','o','s']]).
q([['Z','U','L'],['Y','N','q'],['M','Q','O','K'],['r','M','P']],[['Z','U'],['Y','N','q'],['M','Q','O'],['r','M','P']]).
q([['l','z','N','t'],['w','n','k'],['b','m','l']],[['l','z','N','t'],['w','n','k'],['b','m']]).
