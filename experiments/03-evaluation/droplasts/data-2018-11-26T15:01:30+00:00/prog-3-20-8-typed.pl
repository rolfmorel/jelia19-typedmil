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

my_succ3(A,B):-succ(A,B),B =< 10.
my_msort4(A,B):-msort(A,B).
my_element5(A,B):-member(B,A).
my_tolower6(A,B):-downcase_atom(A,B).
my_head7([H|_],H).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_sumlist11(A,B):-sumlist(A,B).

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

my_toupper13(A,B):-upcase_atom(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_set17(A):-list_to_set(A,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_list_to_set19(A,B):-list_to_set(A,B).
my_uppercase20(A):-upcase_atom(A,A).
my_min_list21(A,B):-min_list(A,B).
my_odd22(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_succ3,[int,int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_element5,[list(T),T]).
prim(my_tolower6,[char,char]).
prim(my_head7,[list(T),T]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len9,[list(_),int]).
prim(my_double10,[int,int]).
prim(my_sumlist11,[list(int),int]).
prim(my_toupper13,[char,char]).
prim(my_lowercase14,[char]).
prim(my_last15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_set17,[list(_)]).
prim(my_pred18,[int,int]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_uppercase20,[char]).
prim(my_min_list21,[list(int),int]).
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
p([['j','C','g','z'],['i','A','B'],['s','X','l','g'],['B','D','J']],[['j','C','g'],['i','A'],['s','X','l'],['B','D']]).
p([['P','O','Q','Q'],['E','h','t','a'],['K','y','S','y']],[['P','O','Q'],['E','h','t'],['K','y','S']]).
p([['c','W','r'],['Q','r','A'],['F','F','U'],['x','L','X']],[['c','W'],['Q','r'],['F','F'],['x','L']]).
p([['j','O','w','y'],['s','W','p'],['J','P','B'],['Q','s','t','k']],[['j','O','w'],['s','W'],['J','P'],['Q','s','t']]).
p([['D','L','P'],['k','y','U']],[['D','L'],['k','y']]).
q([['O','C','H'],['J','g','I'],['s','U','u','G'],['R','p','o','X']],[['O','C','H'],['J','g','I'],['s','U','u','G'],['R','p','o']]).
q([['y','p','z','o'],['F','F','q','t']],[['y','p','z','o'],['F','F','q']]).
q([['b','m','W'],['U','I','T']],[['b','m'],['U','I','T']]).
q([['h','o','b','p'],['S','H','p'],['q','X','V']],[['h','o','b','p'],['S','H','p'],['q','X']]).
q([['L','D','u','Y'],['E','y','s'],['r','q','D','c']],[['L','D','u','Y'],['E','y'],['r','q','D','c']]).
