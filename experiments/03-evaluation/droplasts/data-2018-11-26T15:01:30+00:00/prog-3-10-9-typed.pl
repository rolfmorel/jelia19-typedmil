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

my_sumlist3(A,B):-sumlist(A,B).
my_msort4(A,B):-msort(A,B).

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

my_even6(A):-0 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_len8(A,B):-length(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_min_list10(A,B):-min_list(A,B).
my_odd11(A):-1 is A mod 2.
my_last12(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_even6,[int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_len8,[list(_),int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_min_list10,[list(int),int]).
prim(my_odd11,[int]).
prim(my_last12,[list(T),T]).
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
p([['y','Z','l','P'],['N','g','T']],[['y','Z','l'],['N','g']]).
p([['m','S','Y'],['t','l','j','W'],['J','i','H','W'],['S','W','r']],[['m','S'],['t','l','j'],['J','i','H'],['S','W']]).
p([['m','H','z','c'],['R','W','C','u'],['n','h','s','H'],['A','i','h','H']],[['m','H','z'],['R','W','C'],['n','h','s'],['A','i','h']]).
p([['H','A','F','v'],['x','X','N','k']],[['H','A','F'],['x','X','N']]).
p([['e','C','Y'],['A','I','O'],['a','e','c']],[['e','C'],['A','I'],['a','e']]).
q([['E','E','n'],['E','W','c']],[['E','E'],['E','W','c']]).
q([['t','C','V','H'],['a','R','r','R'],['y','E','b','r'],['X','h','H','y']],[['t','C','V'],['a','R','r','R'],['y','E','b','r'],['X','h','H','y']]).
q([['r','v','n'],['T','V','c','H'],['c','t','Q','Y'],['b','U','t']],[['r','v'],['T','V','c','H'],['c','t','Q','Y'],['b','U','t']]).
q([['W','E','X'],['P','g','r'],['I','J','d','g'],['s','u','X']],[['W','E'],['P','g','r'],['I','J','d'],['s','u','X']]).
q([['a','c','k'],['N','O','l'],['H','i','i'],['t','e','v','s']],[['a','c','k'],['N','O','l'],['H','i','i'],['t','e','v']]).
