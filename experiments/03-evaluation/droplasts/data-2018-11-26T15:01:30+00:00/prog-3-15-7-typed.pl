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

my_len3(A,B):-length(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_max_list5(A,B):-max_list(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_uppercase7(A):-upcase_atom(A,A).
my_last8(A,B):-last(A,B).
my_odd9(A):-1 is A mod 2.
my_min_list10(A,B):-min_list(A,B).

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

my_set12(A):-list_to_set(A,A).
my_head13([H|_],H).
my_flatten14(A,B):-flatten(A,B).
my_msort15(A,B):-msort(A,B).
my_even16(A):-0 is A mod 2.
my_lowercase17(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_tolower4,[char,char]).
prim(my_max_list5,[list(int),int]).
prim(my_toupper6,[char,char]).
prim(my_uppercase7,[char]).
prim(my_last8,[list(T),T]).
prim(my_odd9,[int]).
prim(my_min_list10,[list(int),int]).
prim(my_set12,[list(_)]).
prim(my_head13,[list(T),T]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_msort15,[list(int),list(int)]).
prim(my_even16,[int]).
prim(my_lowercase17,[char]).
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
p([['V','f','d'],['G','X','l','I'],['g','S','U','X'],['Q','N','b']],[['V','f'],['G','X','l'],['g','S','U'],['Q','N']]).
p([['d','u','H'],['U','U','S','T']],[['d','u'],['U','U','S']]).
p([['G','j','H','d'],['Z','M','A'],['e','V','H','X']],[['G','j','H'],['Z','M'],['e','V','H']]).
p([['E','I','y','i'],['d','n','g']],[['E','I','y'],['d','n']]).
p([['T','D','J'],['u','B','r','x'],['e','e','X','C']],[['T','D'],['u','B','r'],['e','e','X']]).
q([['J','Q','P'],['e','N','R']],[['J','Q','P'],['e','N']]).
q([['W','H','f'],['m','C','T','G'],['Q','M','l','z'],['W','a','q']],[['W','H','f'],['m','C','T'],['Q','M','l','z'],['W','a','q']]).
q([['j','j','b'],['P','i','B'],['p','N','U','j']],[['j','j','b'],['P','i','B'],['p','N','U']]).
q([['T','B','v','I'],['j','n','W','X']],[['T','B','v','I'],['j','n','W']]).
q([['Q','v','q'],['P','j','O'],['Q','z','S','U'],['S','J','Q','i']],[['Q','v'],['P','j','O'],['Q','z','S','U'],['S','J','Q']]).
