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
my_head4([H|_],H).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_uppercase9(A):-upcase_atom(A,A).
my_tolower10(A,B):-downcase_atom(A,B).
my_last11(A,B):-last(A,B).

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

my_sumlist13(A,B):-sumlist(A,B).
my_msort14(A,B):-msort(A,B).
my_list_to_set15(A,B):-list_to_set(A,B).
my_set16(A):-list_to_set(A,A).
my_min_list17(A,B):-min_list(A,B).
my_odd18(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_max_list5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_toupper7,[char,char]).
prim(my_double8,[int,int]).
prim(my_uppercase9,[char]).
prim(my_tolower10,[char,char]).
prim(my_last11,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_set16,[list(_)]).
prim(my_min_list17,[list(int),int]).
prim(my_odd18,[int]).
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
p([['K','X','J','B'],['x','p','l','C']],[['K','X','J'],['x','p','l']]).
p([['Q','G','e'],['N','a','n'],['z','t','M']],[['Q','G'],['N','a'],['z','t']]).
p([['R','Q','r'],['n','A','m'],['o','e','H','u']],[['R','Q'],['n','A'],['o','e','H']]).
p([['O','I','k'],['A','k','Z','K']],[['O','I'],['A','k','Z']]).
p([['z','Z','T','N'],['B','z','c'],['s','c','Y'],['W','U','x']],[['z','Z','T'],['B','z'],['s','c'],['W','U']]).
q([['j','c','h'],['K','d','r'],['D','J','r']],[['j','c','h'],['K','d','r'],['D','J']]).
q([['N','V','p','z'],['q','f','O','f']],[['N','V','p','z'],['q','f','O']]).
q([['U','p','y'],['S','B','e'],['S','U','P','u'],['f','R','D','h']],[['U','p'],['S','B','e'],['S','U','P','u'],['f','R','D']]).
q([['I','A','Z'],['F','L','A'],['M','L','l']],[['I','A'],['F','L','A'],['M','L','l']]).
q([['U','x','l'],['s','X','Y'],['m','N','t'],['g','e','r']],[['U','x'],['s','X','Y'],['m','N','t'],['g','e','r']]).
