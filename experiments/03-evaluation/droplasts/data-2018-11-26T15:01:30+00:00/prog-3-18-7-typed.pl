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
my_toupper4(A,B):-upcase_atom(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_msort6(A,B):-msort(A,B).
my_flatten7(A,B):-flatten(A,B).
my_min_list8(A,B):-min_list(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_set10(A):-list_to_set(A,A).
my_lowercase11(A):-downcase_atom(A,A).
my_max_list12(A,B):-max_list(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_even14(A):-0 is A mod 2.
my_pred15(A,B):-succ(B,A),A > 0.

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

my_list_to_set17(A,B):-list_to_set(A,B).
my_tolower18(A,B):-downcase_atom(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_element20(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_toupper4,[char,char]).
prim(my_succ5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_min_list8,[list(int),int]).
prim(my_double9,[int,int]).
prim(my_set10,[list(_)]).
prim(my_lowercase11,[char]).
prim(my_max_list12,[list(int),int]).
prim(my_uppercase13,[char]).
prim(my_even14,[int]).
prim(my_pred15,[int,int]).
prim(my_list_to_set17,[list(T),list(T)]).
prim(my_tolower18,[char,char]).
prim(my_sumlist19,[list(int),int]).
prim(my_element20,[list(T),T]).
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
p([['e','J','H','F'],['E','k','d','I'],['l','N','O'],['x','n','e','s']],[['e','J','H'],['E','k','d'],['l','N'],['x','n','e']]).
p([['Y','m','m'],['Q','t','X']],[['Y','m'],['Q','t']]).
p([['f','O','i','N'],['C','a','u'],['u','K','E']],[['f','O','i'],['C','a'],['u','K']]).
p([['d','v','Q'],['X','C','G','c'],['r','P','W','y'],['M','g','N']],[['d','v'],['X','C','G'],['r','P','W'],['M','g']]).
p([['Y','x','I','g'],['b','Q','y','G']],[['Y','x','I'],['b','Q','y']]).
q([['O','C','D','R'],['q','K','g'],['x','o','m'],['G','Z','q','X']],[['O','C','D'],['q','K'],['x','o','m'],['G','Z','q','X']]).
q([['G','g','I'],['z','l','q','T']],[['G','g','I'],['z','l','q']]).
q([['o','C','S','v'],['l','s','x','e'],['S','x','w']],[['o','C','S','v'],['l','s','x'],['S','x','w']]).
q([['A','o','z'],['v','n','l','Y'],['Q','K','h'],['r','l','x','H']],[['A','o','z'],['v','n','l'],['Q','K','h'],['r','l','x','H']]).
q([['l','I','W','v'],['X','u','Q','g'],['R','y','O','D'],['J','I','z','h']],[['l','I','W','v'],['X','u','Q'],['R','y','O','D'],['J','I','z','h']]).
