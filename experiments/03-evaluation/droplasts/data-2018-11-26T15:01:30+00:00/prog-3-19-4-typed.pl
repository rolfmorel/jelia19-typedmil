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

my_min_list3(A,B):-min_list(A,B).
my_even4(A):-0 is A mod 2.
my_flatten5(A,B):-flatten(A,B).
my_set6(A):-list_to_set(A,A).
my_max_list7(A,B):-max_list(A,B).
my_lowercase8(A):-downcase_atom(A,A).

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

my_list_to_set10(A,B):-list_to_set(A,B).
my_msort11(A,B):-msort(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_tolower13(A,B):-downcase_atom(A,B).
my_head14([H|_],H).
my_uppercase15(A):-upcase_atom(A,A).
my_len16(A,B):-length(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_odd18(A):-1 is A mod 2.
my_toupper19(A,B):-upcase_atom(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_element21(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_min_list3,[list(int),int]).
prim(my_even4,[int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_set6,[list(_)]).
prim(my_max_list7,[list(int),int]).
prim(my_lowercase8,[char]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_msort11,[list(int),list(int)]).
prim(my_double12,[int,int]).
prim(my_tolower13,[char,char]).
prim(my_head14,[list(T),T]).
prim(my_uppercase15,[char]).
prim(my_len16,[list(_),int]).
prim(my_pred17,[int,int]).
prim(my_odd18,[int]).
prim(my_toupper19,[char,char]).
prim(my_sumlist20,[list(int),int]).
prim(my_element21,[list(T),T]).
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
p([['o','m','y'],['r','h','E'],['c','K','E','S']],[['o','m'],['r','h'],['c','K','E']]).
p([['n','n','T','K'],['s','W','d','c']],[['n','n','T'],['s','W','d']]).
p([['G','S','t'],['h','f','O','Y'],['n','P','h','k']],[['G','S'],['h','f','O'],['n','P','h']]).
p([['C','R','V'],['x','n','k','x']],[['C','R'],['x','n','k']]).
p([['H','g','M'],['v','r','B']],[['H','g'],['v','r']]).
q([['W','U','T','Q'],['D','q','g'],['o','W','y','K'],['F','e','y','i']],[['W','U','T','Q'],['D','q'],['o','W','y'],['F','e','y','i']]).
q([['w','Z','f','G'],['v','z','q','Q'],['m','E','I'],['V','K','a','n']],[['w','Z','f'],['v','z','q','Q'],['m','E'],['V','K','a','n']]).
q([['D','Q','U','z'],['W','n','e','f'],['Y','R','G','D'],['t','G','K','Z']],[['D','Q','U','z'],['W','n','e'],['Y','R','G','D'],['t','G','K']]).
q([['y','v','y','p'],['j','O','M'],['i','E','X','h']],[['y','v','y'],['j','O','M'],['i','E','X','h']]).
q([['N','Y','j','F'],['E','L','I','D'],['O','s','l','b'],['v','Z','i']],[['N','Y','j','F'],['E','L','I','D'],['O','s','l','b'],['v','Z']]).
