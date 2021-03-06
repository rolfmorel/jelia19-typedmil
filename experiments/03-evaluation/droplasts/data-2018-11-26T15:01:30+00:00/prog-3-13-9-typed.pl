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

my_list_to_set5(A,B):-list_to_set(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_min_list7(A,B):-min_list(A,B).
my_flatten8(A,B):-flatten(A,B).
my_element9(A,B):-member(B,A).
my_set10(A):-list_to_set(A,A).
my_double11(N,M):-M is 2*N,M =< 10.
my_head12([H|_],H).
my_sumlist13(A,B):-sumlist(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_len15(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_lowercase3,[char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_tolower6,[char,char]).
prim(my_min_list7,[list(int),int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_element9,[list(T),T]).
prim(my_set10,[list(_)]).
prim(my_double11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_uppercase14,[char]).
prim(my_len15,[list(_),int]).
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
p([['r','i','n','m'],['L','V','J'],['r','T','t','d'],['S','Y','d','u']],[['r','i','n'],['L','V'],['r','T','t'],['S','Y','d']]).
p([['d','h','J'],['Z','J','I'],['W','S','A']],[['d','h'],['Z','J'],['W','S']]).
p([['u','t','A','i'],['d','G','T']],[['u','t','A'],['d','G']]).
p([['M','x','I','M'],['e','N','x','V'],['A','h','m','U'],['b','W','p']],[['M','x','I'],['e','N','x'],['A','h','m'],['b','W']]).
p([['C','v','b','g'],['o','Z','S','N'],['Q','D','K'],['i','l','f']],[['C','v','b'],['o','Z','S'],['Q','D'],['i','l']]).
q([['Z','C','q','e'],['N','t','X','n']],[['Z','C','q','e'],['N','t','X']]).
q([['G','i','w','r'],['D','q','s','n'],['Q','k','X','I'],['g','A','h','I']],[['G','i','w'],['D','q','s'],['Q','k','X','I'],['g','A','h','I']]).
q([['l','H','F'],['g','c','b','A']],[['l','H'],['g','c','b','A']]).
q([['Y','D','I','C'],['T','Z','O','p'],['T','x','A','d']],[['Y','D','I','C'],['T','Z','O','p'],['T','x','A']]).
q([['w','s','R','B'],['A','F','C','h'],['k','H','s','g'],['Y','l','O']],[['w','s','R','B'],['A','F','C','h'],['k','H','s','g'],['Y','l']]).
