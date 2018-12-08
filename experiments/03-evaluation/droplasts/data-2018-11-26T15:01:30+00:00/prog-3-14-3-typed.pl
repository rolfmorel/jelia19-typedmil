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
my_tolower4(A,B):-downcase_atom(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_even7(A):-0 is A mod 2.

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

my_pred9(A,B):-succ(B,A),A > 0.
my_head10([H|_],H).
my_element11(A,B):-member(B,A).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_flatten14(A,B):-flatten(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_sumlist16(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_double3,[int,int]).
prim(my_tolower4,[char,char]).
prim(my_toupper5,[char,char]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_even7,[int]).
prim(my_pred9,[int,int]).
prim(my_head10,[list(T),T]).
prim(my_element11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_len13,[list(_),int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_uppercase15,[char]).
prim(my_sumlist16,[list(int),int]).
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
p([['T','G','l'],['K','e','d','i'],['n','B','z']],[['T','G'],['K','e','d'],['n','B']]).
p([['Q','R','f'],['D','P','g','w']],[['Q','R'],['D','P','g']]).
p([['k','m','x','C'],['M','X','T','H'],['S','W','k'],['L','K','b','d']],[['k','m','x'],['M','X','T'],['S','W'],['L','K','b']]).
p([['Q','H','t'],['s','H','o']],[['Q','H'],['s','H']]).
p([['o','m','X'],['k','J','V','j'],['N','o','A','L'],['o','j','o']],[['o','m'],['k','J','V'],['N','o','A'],['o','j']]).
q([['d','e','y','y'],['e','Y','z'],['G','J','x','v'],['y','E','l']],[['d','e','y'],['e','Y','z'],['G','J','x','v'],['y','E','l']]).
q([['h','Y','q'],['u','Z','Z','p'],['i','f','V'],['H','j','h']],[['h','Y','q'],['u','Z','Z','p'],['i','f'],['H','j','h']]).
q([['B','z','u'],['f','T','Z','V']],[['B','z'],['f','T','Z','V']]).
q([['o','o','c','N'],['i','d','C','X']],[['o','o','c'],['i','d','C','X']]).
q([['s','z','e','X'],['V','r','d'],['v','u','f','A']],[['s','z','e'],['V','r','d'],['v','u','f','A']]).
