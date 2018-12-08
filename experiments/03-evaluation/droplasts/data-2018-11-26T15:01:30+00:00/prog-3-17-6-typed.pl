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

my_min_list5(A,B):-min_list(A,B).
my_odd6(A):-1 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_element10(A,B):-member(B,A).
my_head11([H|_],H).
my_flatten12(A,B):-flatten(A,B).
my_max_list13(A,B):-max_list(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_list_to_set16(A,B):-list_to_set(A,B).
my_even17(A):-0 is A mod 2.
my_lowercase18(A):-downcase_atom(A,A).
my_set19(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_pred7,[int,int]).
prim(my_last8,[list(T),T]).
prim(my_succ9,[int,int]).
prim(my_element10,[list(T),T]).
prim(my_head11,[list(T),T]).
prim(my_flatten12,[list(list(T)),list(T)]).
prim(my_max_list13,[list(int),int]).
prim(my_toupper14,[char,char]).
prim(my_double15,[int,int]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_even17,[int]).
prim(my_lowercase18,[char]).
prim(my_set19,[list(_)]).
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
p([['B','w','f','d'],['o','v','R','f'],['L','O','x']],[['B','w','f'],['o','v','R'],['L','O']]).
p([['j','F','a'],['x','C','i'],['j','i','V','B'],['o','J','e','b']],[['j','F'],['x','C'],['j','i','V'],['o','J','e']]).
p([['q','R','G','I'],['t','m','S','L'],['Y','w','Q','j'],['I','O','L','J']],[['q','R','G'],['t','m','S'],['Y','w','Q'],['I','O','L']]).
p([['Q','y','o','N'],['B','D','k'],['z','a','V','U'],['J','D','a','u']],[['Q','y','o'],['B','D'],['z','a','V'],['J','D','a']]).
p([['S','c','m'],['I','q','i'],['W','N','f','F']],[['S','c'],['I','q'],['W','N','f']]).
q([['o','y','Q','P'],['p','K','O'],['Y','k','H','Y'],['R','h','g']],[['o','y','Q'],['p','K'],['Y','k','H','Y'],['R','h','g']]).
q([['A','D','T','z'],['c','s','P','C']],[['A','D','T'],['c','s','P','C']]).
q([['I','e','e','A'],['y','u','Q'],['T','d','n'],['b','U','V']],[['I','e','e','A'],['y','u'],['T','d','n'],['b','U','V']]).
q([['w','o','O','s'],['J','Z','k']],[['w','o','O','s'],['J','Z']]).
q([['U','J','E','A'],['v','H','X'],['E','F','D']],[['U','J','E'],['v','H','X'],['E','F','D']]).
