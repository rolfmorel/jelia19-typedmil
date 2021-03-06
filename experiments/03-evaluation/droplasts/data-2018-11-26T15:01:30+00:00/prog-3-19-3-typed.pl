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

my_tolower3(A,B):-downcase_atom(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_toupper5(A,B):-upcase_atom(A,B).
my_msort6(A,B):-msort(A,B).
my_odd7(A):-1 is A mod 2.
my_uppercase8(A):-upcase_atom(A,A).
my_element9(A,B):-member(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_last11(A,B):-last(A,B).
my_set12(A):-list_to_set(A,A).
my_len13(A,B):-length(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_flatten15(A,B):-flatten(A,B).
my_max_list16(A,B):-max_list(A,B).
my_lowercase17(A):-downcase_atom(A,A).

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

my_even19(A):-0 is A mod 2.
my_head20([H|_],H).
my_list_to_set21(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_tolower3,[char,char]).
prim(my_double4,[int,int]).
prim(my_toupper5,[char,char]).
prim(my_msort6,[list(int),list(int)]).
prim(my_odd7,[int]).
prim(my_uppercase8,[char]).
prim(my_element9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_last11,[list(T),T]).
prim(my_set12,[list(_)]).
prim(my_len13,[list(_),int]).
prim(my_pred14,[int,int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_max_list16,[list(int),int]).
prim(my_lowercase17,[char]).
prim(my_even19,[int]).
prim(my_head20,[list(T),T]).
prim(my_list_to_set21,[list(T),list(T)]).
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
p([['C','B','H'],['z','H','H','P'],['M','o','V','v'],['x','r','A','D']],[['C','B'],['z','H','H'],['M','o','V'],['x','r','A']]).
p([['F','G','G'],['n','D','l','A'],['l','o','i'],['y','B','m','Q']],[['F','G'],['n','D','l'],['l','o'],['y','B','m']]).
p([['D','S','L','h'],['e','U','T','o']],[['D','S','L'],['e','U','T']]).
p([['Y','Y','Y','g'],['Z','A','B'],['u','x','Q','h'],['k','y','r','g']],[['Y','Y','Y'],['Z','A'],['u','x','Q'],['k','y','r']]).
p([['G','t','r'],['R','y','x'],['e','J','o','e'],['V','P','v']],[['G','t'],['R','y'],['e','J','o'],['V','P']]).
q([['G','b','N','P'],['R','Y','t','a']],[['G','b','N','P'],['R','Y','t']]).
q([['g','A','m'],['B','S','Q'],['x','G','e']],[['g','A','m'],['B','S','Q'],['x','G']]).
q([['M','O','I','W'],['i','X','P','D'],['G','T','r','P']],[['M','O','I','W'],['i','X','P'],['G','T','r','P']]).
q([['a','Z','F','U'],['X','r','M','a'],['X','x','J','S']],[['a','Z','F','U'],['X','r','M'],['X','x','J','S']]).
q([['l','c','i','Y'],['o','l','Z'],['u','V','n','l'],['i','K','B']],[['l','c','i','Y'],['o','l','Z'],['u','V','n'],['i','K']]).
