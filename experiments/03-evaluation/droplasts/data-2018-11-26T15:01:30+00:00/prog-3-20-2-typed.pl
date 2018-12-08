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

my_set4(A):-list_to_set(A,A).
my_uppercase5(A):-upcase_atom(A,A).
my_pred6(A,B):-succ(B,A),A > 0.
my_max_list7(A,B):-max_list(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_min_list9(A,B):-min_list(A,B).
my_tolower10(A,B):-downcase_atom(A,B).
my_flatten11(A,B):-flatten(A,B).
my_len12(A,B):-length(A,B).
my_head13([H|_],H).
my_lowercase14(A):-downcase_atom(A,A).
my_double15(N,M):-M is 2*N,M =< 10.
my_element16(A,B):-member(B,A).
my_even17(A):-0 is A mod 2.
my_sumlist18(A,B):-sumlist(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
my_msort20(A,B):-msort(A,B).
my_last21(A,B):-last(A,B).
my_odd22(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set4,[list(_)]).
prim(my_uppercase5,[char]).
prim(my_pred6,[int,int]).
prim(my_max_list7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_min_list9,[list(int),int]).
prim(my_tolower10,[char,char]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_len12,[list(_),int]).
prim(my_head13,[list(T),T]).
prim(my_lowercase14,[char]).
prim(my_double15,[int,int]).
prim(my_element16,[list(T),T]).
prim(my_even17,[int]).
prim(my_sumlist18,[list(int),int]).
prim(my_toupper19,[char,char]).
prim(my_msort20,[list(int),list(int)]).
prim(my_last21,[list(T),T]).
prim(my_odd22,[int]).
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
p([['g','R','E','d'],['f','c','V'],['i','G','i','k']],[['g','R','E'],['f','c'],['i','G','i']]).
p([['N','H','b'],['U','A','z','v'],['P','m','z','P']],[['N','H'],['U','A','z'],['P','m','z']]).
p([['I','v','G'],['I','C','n'],['f','h','H','Z']],[['I','v'],['I','C'],['f','h','H']]).
p([['r','b','t','b'],['j','W','e']],[['r','b','t'],['j','W']]).
p([['R','o','T','n'],['X','e','Y']],[['R','o','T'],['X','e']]).
q([['N','T','e'],['K','u','z'],['d','V','e']],[['N','T','e'],['K','u','z'],['d','V']]).
q([['I','g','T','z'],['l','U','B','B'],['q','X','g'],['w','N','c']],[['I','g','T'],['l','U','B','B'],['q','X'],['w','N','c']]).
q([['O','C','w'],['N','K','E','D']],[['O','C','w'],['N','K','E']]).
q([['X','z','D','P'],['b','X','a','Q'],['o','E','C','y'],['t','U','F']],[['X','z','D','P'],['b','X','a'],['o','E','C','y'],['t','U','F']]).
q([['B','h','B'],['N','d','a'],['e','H','y','X']],[['B','h'],['N','d','a'],['e','H','y','X']]).
