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

my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_max_list6(A,B):-max_list(A,B).
my_head7([H|_],H).
my_even8(A):-0 is A mod 2.
my_toupper9(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_last3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_tolower5,[char,char]).
prim(my_max_list6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_even8,[int]).
prim(my_toupper9,[char,char]).
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
p([['z','W','J'],['g','M','c','R'],['O','S','N','G']],[['z','W'],['g','M','c'],['O','S','N']]).
p([['H','j','J'],['l','c','B'],['I','D','n','P'],['i','o','g']],[['H','j'],['l','c'],['I','D','n'],['i','o']]).
p([['s','Q','X','E'],['A','B','Y','z']],[['s','Q','X'],['A','B','Y']]).
p([['n','j','z'],['H','u','r']],[['n','j'],['H','u']]).
p([['Y','P','s'],['Q','W','u']],[['Y','P'],['Q','W']]).
q([['b','f','j','s'],['G','G','k','B']],[['b','f','j','s'],['G','G','k']]).
q([['s','t','c'],['g','U','Z']],[['s','t'],['g','U','Z']]).
q([['l','l','f'],['A','l','C'],['y','n','B','S']],[['l','l','f'],['A','l','C'],['y','n','B']]).
q([['p','o','b','C'],['w','n','D']],[['p','o','b','C'],['w','n']]).
q([['c','g','p','T'],['S','A','c'],['p','j','i']],[['c','g','p'],['S','A','c'],['p','j','i']]).
