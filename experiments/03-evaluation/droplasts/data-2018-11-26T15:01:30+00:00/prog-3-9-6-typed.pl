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

my_odd3(A):-1 is A mod 2.
my_list_to_set4(A,B):-list_to_set(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_tolower6(A,B):-downcase_atom(A,B).
my_set7(A):-list_to_set(A,A).
my_lowercase8(A):-downcase_atom(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_flatten10(A,B):-flatten(A,B).
my_msort11(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_tolower6,[char,char]).
prim(my_set7,[list(_)]).
prim(my_lowercase8,[char]).
prim(my_pred9,[int,int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_msort11,[list(int),list(int)]).
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
p([['B','N','l','v'],['g','S','p']],[['B','N','l'],['g','S']]).
p([['Q','Q','K'],['U','Y','L']],[['Q','Q'],['U','Y']]).
p([['k','v','n','b'],['f','e','h'],['N','A','w','m']],[['k','v','n'],['f','e'],['N','A','w']]).
p([['v','V','Q'],['W','F','O'],['X','y','M','J'],['z','F','n']],[['v','V'],['W','F'],['X','y','M'],['z','F']]).
p([['S','E','v'],['C','s','J','D']],[['S','E'],['C','s','J']]).
q([['m','o','B'],['r','I','s'],['I','u','a'],['d','o','s','O']],[['m','o','B'],['r','I','s'],['I','u'],['d','o','s','O']]).
q([['Q','W','n'],['H','M','E','a'],['h','f','q','h'],['v','t','u','B']],[['Q','W','n'],['H','M','E','a'],['h','f','q'],['v','t','u','B']]).
q([['W','T','v'],['D','G','K','A'],['d','E','i']],[['W','T','v'],['D','G','K','A'],['d','E']]).
q([['w','c','Z'],['K','I','c'],['V','g','h','G'],['G','d','p']],[['w','c','Z'],['K','I','c'],['V','g','h','G'],['G','d']]).
q([['T','D','V','x'],['d','p','J']],[['T','D','V'],['d','p','J']]).
