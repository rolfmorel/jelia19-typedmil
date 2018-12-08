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
my_double4(N,M):-M is 2*N,M =< 10.
my_tolower5(A,B):-downcase_atom(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_max_list7(A,B):-max_list(A,B).
my_msort8(A,B):-msort(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_double4,[int,int]).
prim(my_tolower5,[char,char]).
prim(my_succ6,[int,int]).
prim(my_max_list7,[list(int),int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_toupper9,[char,char]).
prim(my_list_to_set10,[list(T),list(T)]).
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
p([['z','b','m','v'],['d','Z','b']],[['z','b','m'],['d','Z']]).
p([['r','X','Q'],['z','c','U','u'],['R','m','R']],[['r','X'],['z','c','U'],['R','m']]).
p([['Z','F','R','f'],['u','Y','P','R'],['z','p','v'],['M','Y','M']],[['Z','F','R'],['u','Y','P'],['z','p'],['M','Y']]).
p([['x','q','e'],['m','N','Y'],['r','b','Z'],['q','V','T','X']],[['x','q'],['m','N'],['r','b'],['q','V','T']]).
p([['E','X','K'],['J','P','E','W']],[['E','X'],['J','P','E']]).
q([['d','I','E'],['H','n','v']],[['d','I'],['H','n','v']]).
q([['d','a','j'],['T','N','J']],[['d','a','j'],['T','N']]).
q([['j','o','c'],['g','k','q'],['A','z','N']],[['j','o','c'],['g','k'],['A','z','N']]).
q([['k','S','a'],['C','S','C','F'],['j','D','u']],[['k','S'],['C','S','C','F'],['j','D','u']]).
q([['t','O','S'],['B','V','u','i']],[['t','O'],['B','V','u','i']]).
