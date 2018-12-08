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

my_set5(A):-list_to_set(A,A).
my_even6(A):-0 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_succ9(A,B):-succ(A,B),B =< 10.
my_max_list10(A,B):-max_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_element12(A,B):-member(B,A).
my_head13([H|_],H).
my_min_list14(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_set5,[list(_)]).
prim(my_even6,[int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_pred8,[int,int]).
prim(my_succ9,[int,int]).
prim(my_max_list10,[list(int),int]).
prim(my_tolower11,[char,char]).
prim(my_element12,[list(T),T]).
prim(my_head13,[list(T),T]).
prim(my_min_list14,[list(int),int]).
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
p([['c','N','X'],['s','A','I'],['x','d','s']],[['c','N'],['s','A'],['x','d']]).
p([['K','Y','Q','I'],['z','M','u','t']],[['K','Y','Q'],['z','M','u']]).
p([['B','T','s','d'],['y','A','E','N'],['r','l','N'],['k','S','X']],[['B','T','s'],['y','A','E'],['r','l'],['k','S']]).
p([['B','C','h'],['L','o','s'],['Y','c','K','a']],[['B','C'],['L','o'],['Y','c','K']]).
p([['P','E','R'],['b','N','D','P'],['c','X','C','l']],[['P','E'],['b','N','D'],['c','X','C']]).
q([['u','B','j'],['v','r','b','i']],[['u','B','j'],['v','r','b']]).
q([['O','T','u','K'],['M','Z','J'],['G','X','B']],[['O','T','u'],['M','Z','J'],['G','X','B']]).
q([['X','R','U'],['B','q','J'],['z','I','e']],[['X','R'],['B','q','J'],['z','I','e']]).
q([['o','z','F','p'],['Y','V','b'],['W','i','y']],[['o','z','F','p'],['Y','V'],['W','i','y']]).
q([['J','C','P'],['T','S','t']],[['J','C','P'],['T','S']]).
