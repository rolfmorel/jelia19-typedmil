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
my_pred4(A,B):-succ(B,A),A > 0.
my_min_list5(A,B):-min_list(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).

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

my_toupper8(A,B):-upcase_atom(A,B).
my_uppercase9(A):-upcase_atom(A,A).
my_double10(N,M):-M is 2*N,M =< 10.
my_element11(A,B):-member(B,A).
my_msort12(A,B):-msort(A,B).
my_odd13(A):-1 is A mod 2.
my_head14([H|_],H).
my_sumlist15(A,B):-sumlist(A,B).
my_set16(A):-list_to_set(A,A).
my_max_list17(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_pred4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_toupper8,[char,char]).
prim(my_uppercase9,[char]).
prim(my_double10,[int,int]).
prim(my_element11,[list(T),T]).
prim(my_msort12,[list(int),list(int)]).
prim(my_odd13,[int]).
prim(my_head14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_set16,[list(_)]).
prim(my_max_list17,[list(int),int]).
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
p([['h','S','b','B'],['m','a','N']],[['h','S','b'],['m','a']]).
p([['N','z','d','o'],['X','Q','T','V']],[['N','z','d'],['X','Q','T']]).
p([['e','E','z'],['k','I','x'],['X','Y','N'],['C','r','m','O']],[['e','E'],['k','I'],['X','Y'],['C','r','m']]).
p([['G','A','h'],['g','r','c']],[['G','A'],['g','r']]).
p([['e','u','a'],['w','L','c','m'],['G','y','j']],[['e','u'],['w','L','c'],['G','y']]).
q([['V','e','X','b'],['a','x','X','E'],['I','K','c','Q']],[['V','e','X'],['a','x','X','E'],['I','K','c','Q']]).
q([['S','v','y'],['f','d','Q','P'],['a','A','J','y']],[['S','v','y'],['f','d','Q','P'],['a','A','J']]).
q([['b','A','s','A'],['X','T','C'],['U','x','x'],['s','P','w']],[['b','A','s','A'],['X','T','C'],['U','x'],['s','P']]).
q([['c','d','E','F'],['q','W','c']],[['c','d','E'],['q','W','c']]).
q([['p','V','x','D'],['h','z','p'],['I','l','c']],[['p','V','x','D'],['h','z','p'],['I','l']]).
