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
my_head4([H|_],H).

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

my_pred6(A,B):-succ(B,A),A > 0.
my_tolower7(A,B):-downcase_atom(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_uppercase9(A):-upcase_atom(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_list_to_set12(A,B):-list_to_set(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_msort14(A,B):-msort(A,B).
my_odd15(A):-1 is A mod 2.
my_flatten16(A,B):-flatten(A,B).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_element19(A,B):-member(B,A).
my_even20(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_min_list3,[list(int),int]).
prim(my_head4,[list(T),T]).
prim(my_pred6,[int,int]).
prim(my_tolower7,[char,char]).
prim(my_double8,[int,int]).
prim(my_uppercase9,[char]).
prim(my_toupper10,[char,char]).
prim(my_lowercase11,[char]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_sumlist13,[list(int),int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_odd15,[int]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_len17,[list(_),int]).
prim(my_last18,[list(T),T]).
prim(my_element19,[list(T),T]).
prim(my_even20,[int]).
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
p([['p','y','i','n'],['n','Q','J'],['S','n','c','Z']],[['p','y','i'],['n','Q'],['S','n','c']]).
p([['Y','M','S','M'],['y','o','b','e'],['m','W','o'],['D','r','r','t']],[['Y','M','S'],['y','o','b'],['m','W'],['D','r','r']]).
p([['v','s','P','d'],['a','l','o','O'],['m','G','N','K']],[['v','s','P'],['a','l','o'],['m','G','N']]).
p([['Z','T','h'],['G','D','E'],['L','Q','b'],['c','G','Z']],[['Z','T'],['G','D'],['L','Q'],['c','G']]).
p([['u','v','E','n'],['W','f','w','v']],[['u','v','E'],['W','f','w']]).
q([['s','l','B','V'],['X','K','h','h'],['H','Z','r'],['r','U','Z','F']],[['s','l','B','V'],['X','K','h'],['H','Z','r'],['r','U','Z','F']]).
q([['f','D','z'],['w','v','O','T'],['L','A','T','X'],['s','I','p']],[['f','D','z'],['w','v','O'],['L','A','T','X'],['s','I','p']]).
q([['n','y','M'],['v','s','S'],['V','i','Y','F']],[['n','y'],['v','s','S'],['V','i','Y','F']]).
q([['G','Y','R'],['b','v','W','a'],['k','U','H'],['D','K','g']],[['G','Y','R'],['b','v','W'],['k','U','H'],['D','K','g']]).
q([['U','o','w'],['j','d','K','W'],['Z','j','G'],['Q','v','b','O']],[['U','o'],['j','d','K','W'],['Z','j'],['Q','v','b','O']]).
