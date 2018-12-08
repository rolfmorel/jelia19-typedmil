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

my_element3(A,B):-member(B,A).
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

my_max_list6(A,B):-max_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_lowercase11(A):-downcase_atom(A,A).
my_double12(N,M):-M is 2*N,M =< 10.
my_odd13(A):-1 is A mod 2.
my_head14([H|_],H).
my_even15(A):-0 is A mod 2.
my_set16(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_max_list6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_toupper8,[char,char]).
prim(my_tolower9,[char,char]).
prim(my_pred10,[int,int]).
prim(my_lowercase11,[char]).
prim(my_double12,[int,int]).
prim(my_odd13,[int]).
prim(my_head14,[list(T),T]).
prim(my_even15,[int]).
prim(my_set16,[list(_)]).
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
p([['O','r','s'],['d','K','j','S'],['n','t','n']],[['O','r'],['d','K','j'],['n','t']]).
p([['n','V','e'],['p','a','d','B']],[['n','V'],['p','a','d']]).
p([['C','x','R'],['o','a','s']],[['C','x'],['o','a']]).
p([['Z','U','Y'],['x','G','N'],['r','f','t','o']],[['Z','U'],['x','G'],['r','f','t']]).
p([['Y','a','N'],['e','S','f','p'],['P','w','g','c']],[['Y','a'],['e','S','f'],['P','w','g']]).
q([['u','H','N','U'],['B','t','C','F'],['f','e','g']],[['u','H','N'],['B','t','C','F'],['f','e','g']]).
q([['Q','M','b'],['G','d','B','u'],['z','e','G']],[['Q','M'],['G','d','B','u'],['z','e','G']]).
q([['L','Y','d'],['l','s','y','G']],[['L','Y','d'],['l','s','y']]).
q([['a','T','A'],['K','f','C','J'],['X','D','W']],[['a','T','A'],['K','f','C'],['X','D','W']]).
q([['O','p','k','w'],['k','v','n','x'],['q','l','J','H'],['D','l','K']],[['O','p','k'],['k','v','n','x'],['q','l','J','H'],['D','l','K']]).
