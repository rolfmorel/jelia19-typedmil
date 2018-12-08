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

my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_list_to_set7(A,B):-list_to_set(A,B).
my_uppercase8(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len4,[list(_),int]).
prim(my_max_list5,[list(int),int]).
prim(my_double6,[int,int]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_uppercase8,[char]).
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
p([['w','G','H'],['j','J','X','u']],[['w','G'],['j','J','X']]).
p([['S','S','D','n'],['K','X','Z','v']],[['S','S','D'],['K','X','Z']]).
p([['T','I','e','U'],['c','L','t','m'],['X','L','i','s'],['T','F','D','A']],[['T','I','e'],['c','L','t'],['X','L','i'],['T','F','D']]).
p([['L','z','i','M'],['X','D','O','R'],['B','y','s']],[['L','z','i'],['X','D','O'],['B','y']]).
p([['s','h','s'],['c','S','C','f'],['t','g','s']],[['s','h'],['c','S','C'],['t','g']]).
q([['i','Q','P','w'],['X','D','C'],['G','K','X']],[['i','Q','P','w'],['X','D'],['G','K','X']]).
q([['t','c','N'],['d','E','L'],['R','Q','p','J']],[['t','c','N'],['d','E'],['R','Q','p','J']]).
q([['x','B','c'],['R','u','X','o']],[['x','B','c'],['R','u','X']]).
q([['y','y','A'],['G','v','u','U'],['X','Z','S','r'],['A','j','S']],[['y','y','A'],['G','v','u','U'],['X','Z','S','r'],['A','j']]).
q([['s','h','q'],['j','V','Q'],['A','p','j','t']],[['s','h'],['j','V','Q'],['A','p','j','t']]).
