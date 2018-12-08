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

my_double3(N,M):-M is 2*N,M =< 10.
my_succ4(A,B):-succ(A,B),B =< 10.
my_element5(A,B):-member(B,A).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_even10(A):-0 is A mod 2.
my_last11(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_double3,[int,int]).
prim(my_succ4,[int,int]).
prim(my_element5,[list(T),T]).
prim(my_len6,[list(_),int]).
prim(my_max_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_toupper9,[char,char]).
prim(my_even10,[int]).
prim(my_last11,[list(T),T]).
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
p([['n','t','K','x'],['T','j','J','L'],['V','w','J'],['I','D','N']],[['n','t','K'],['T','j','J'],['V','w'],['I','D']]).
p([['o','G','O','f'],['m','F','n','Y']],[['o','G','O'],['m','F','n']]).
p([['Y','o','m'],['X','q','b','O'],['d','e','M'],['B','L','m']],[['Y','o'],['X','q','b'],['d','e'],['B','L']]).
p([['F','q','m','A'],['Q','d','e','p']],[['F','q','m'],['Q','d','e']]).
p([['X','i','k'],['J','M','B'],['g','M','u','G']],[['X','i'],['J','M'],['g','M','u']]).
q([['U','Q','w','h'],['r','G','R','k']],[['U','Q','w'],['r','G','R','k']]).
q([['f','a','k'],['S','W','B','O'],['e','D','n']],[['f','a'],['S','W','B','O'],['e','D','n']]).
q([['S','J','P'],['V','G','j']],[['S','J'],['V','G','j']]).
q([['h','t','t','t'],['j','o','T','X'],['T','J','I','U'],['w','U','q']],[['h','t','t'],['j','o','T','X'],['T','J','I','U'],['w','U','q']]).
q([['F','C','Q','C'],['y','i','t','E']],[['F','C','Q'],['y','i','t','E']]).
