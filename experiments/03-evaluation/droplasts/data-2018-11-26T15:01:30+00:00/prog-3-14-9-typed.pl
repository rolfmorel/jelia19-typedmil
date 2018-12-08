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

my_pred3(A,B):-succ(B,A),A > 0.

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

my_max_list5(A,B):-max_list(A,B).
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_flatten9(A,B):-flatten(A,B).
my_element10(A,B):-member(B,A).
my_last11(A,B):-last(A,B).
my_min_list12(A,B):-min_list(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_msort14(A,B):-msort(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_set6,[list(_)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_tolower8,[char,char]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_element10,[list(T),T]).
prim(my_last11,[list(T),T]).
prim(my_min_list12,[list(int),int]).
prim(my_toupper13,[char,char]).
prim(my_msort14,[list(int),list(int)]).
prim(my_head15,[list(T),T]).
prim(my_len16,[list(_),int]).
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
p([['e','M','e','S'],['t','s','O','j'],['s','S','x']],[['e','M','e'],['t','s','O'],['s','S']]).
p([['k','V','i','F'],['j','n','O','F'],['P','t','E','m']],[['k','V','i'],['j','n','O'],['P','t','E']]).
p([['g','n','c'],['s','u','m','u'],['S','v','A','p'],['k','y','b']],[['g','n'],['s','u','m'],['S','v','A'],['k','y']]).
p([['z','v','n','m'],['F','a','g','C'],['w','J','H','R']],[['z','v','n'],['F','a','g'],['w','J','H']]).
p([['k','L','g'],['j','l','j','p'],['n','o','b','Y']],[['k','L'],['j','l','j'],['n','o','b']]).
q([['E','X','P'],['b','p','s','N'],['I','U','a']],[['E','X'],['b','p','s','N'],['I','U','a']]).
q([['i','N','a','L'],['W','t','Q','h']],[['i','N','a','L'],['W','t','Q']]).
q([['E','Q','p'],['f','n','z'],['J','Y','e']],[['E','Q','p'],['f','n'],['J','Y','e']]).
q([['i','l','E'],['x','K','p','j'],['M','P','Y'],['T','n','k','f']],[['i','l'],['x','K','p','j'],['M','P'],['T','n','k','f']]).
q([['d','U','e','v'],['s','s','S','u']],[['d','U','e','v'],['s','s','S']]).
