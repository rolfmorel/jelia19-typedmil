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
my_uppercase4(A):-upcase_atom(A,A).
my_set5(A):-list_to_set(A,A).
my_lowercase6(A):-downcase_atom(A,A).

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

my_pred8(A,B):-succ(B,A),A > 0.
my_odd9(A):-1 is A mod 2.
my_double10(N,M):-M is 2*N,M =< 10.
my_head11([H|_],H).
my_msort12(A,B):-msort(A,B).
my_succ13(A,B):-succ(A,B),B =< 10.
my_max_list14(A,B):-max_list(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_tolower16(A,B):-downcase_atom(A,B).
my_min_list17(A,B):-min_list(A,B).
my_element18(A,B):-member(B,A).
my_flatten19(A,B):-flatten(A,B).
my_even20(A):-0 is A mod 2.
my_len21(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_uppercase4,[char]).
prim(my_set5,[list(_)]).
prim(my_lowercase6,[char]).
prim(my_pred8,[int,int]).
prim(my_odd9,[int]).
prim(my_double10,[int,int]).
prim(my_head11,[list(T),T]).
prim(my_msort12,[list(int),list(int)]).
prim(my_succ13,[int,int]).
prim(my_max_list14,[list(int),int]).
prim(my_toupper15,[char,char]).
prim(my_tolower16,[char,char]).
prim(my_min_list17,[list(int),int]).
prim(my_element18,[list(T),T]).
prim(my_flatten19,[list(list(T)),list(T)]).
prim(my_even20,[int]).
prim(my_len21,[list(_),int]).
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
p([['m','r','T'],['d','Z','G'],['D','c','j'],['L','k','T']],[['m','r'],['d','Z'],['D','c'],['L','k']]).
p([['l','A','c'],['w','f','Y'],['O','P','m','h'],['e','m','W']],[['l','A'],['w','f'],['O','P','m'],['e','m']]).
p([['V','C','R','b'],['o','I','D'],['A','t','y','x']],[['V','C','R'],['o','I'],['A','t','y']]).
p([['w','M','n'],['A','Z','t'],['R','J','x','L'],['F','l','E','N']],[['w','M'],['A','Z'],['R','J','x'],['F','l','E']]).
p([['h','d','n'],['T','o','B'],['d','j','h']],[['h','d'],['T','o'],['d','j']]).
q([['V','a','R'],['C','G','k','W'],['V','O','y'],['P','z','s','k']],[['V','a'],['C','G','k'],['V','O','y'],['P','z','s','k']]).
q([['z','D','F','s'],['m','L','i'],['i','V','D']],[['z','D','F','s'],['m','L','i'],['i','V']]).
q([['k','a','w','W'],['Y','U','Z'],['V','I','G','f']],[['k','a','w','W'],['Y','U'],['V','I','G','f']]).
q([['q','l','r'],['f','G','Q','M'],['Z','O','W'],['c','T','u','u']],[['q','l'],['f','G','Q'],['Z','O','W'],['c','T','u','u']]).
q([['c','e','U','A'],['g','r','F'],['j','m','s','V']],[['c','e','U'],['g','r','F'],['j','m','s','V']]).
