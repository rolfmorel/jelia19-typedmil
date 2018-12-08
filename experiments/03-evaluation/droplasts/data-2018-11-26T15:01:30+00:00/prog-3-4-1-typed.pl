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

my_succ3(A,B):-succ(A,B),B =< 10.
my_len4(A,B):-length(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_tolower6(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_succ3,[int,int]).
prim(my_len4,[list(_),int]).
prim(my_lowercase5,[char]).
prim(my_tolower6,[char,char]).
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
p([['M','A','O'],['F','N','I','M'],['j','f','A','y']],[['M','A'],['F','N','I'],['j','f','A']]).
p([['X','t','H'],['G','T','J','h'],['B','p','B']],[['X','t'],['G','T','J'],['B','p']]).
p([['L','o','t','z'],['N','j','T','G']],[['L','o','t'],['N','j','T']]).
p([['t','y','f'],['p','n','o','R'],['q','m','N','D'],['x','j','J']],[['t','y'],['p','n','o'],['q','m','N'],['x','j']]).
p([['P','y','n'],['Q','k','c'],['Q','L','E','y']],[['P','y'],['Q','k'],['Q','L','E']]).
q([['A','c','G'],['N','q','A','y']],[['A','c'],['N','q','A','y']]).
q([['o','a','t'],['v','P','m','D']],[['o','a','t'],['v','P','m']]).
q([['d','h','m'],['x','k','b']],[['d','h'],['x','k','b']]).
q([['J','A','q','P'],['Y','w','u']],[['J','A','q','P'],['Y','w']]).
q([['V','F','p'],['G','k','v','G'],['G','o','r','q'],['O','y','u','R']],[['V','F'],['G','k','v','G'],['G','o','r'],['O','y','u','R']]).
