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

my_set3(A):-list_to_set(A,A).
my_even4(A):-0 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_msort6(A,B):-msort(A,B).
my_odd7(A):-1 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_head11([H|_],H).
my_sumlist12(A,B):-sumlist(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_toupper14(A,B):-upcase_atom(A,B).
my_max_list15(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_even4,[int]).
prim(my_min_list5,[list(int),int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_odd7,[int]).
prim(my_lowercase8,[char]).
prim(my_tolower9,[char,char]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_head11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_uppercase13,[char]).
prim(my_toupper14,[char,char]).
prim(my_max_list15,[list(int),int]).
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
p([['s','Y','B','u'],['M','x','i']],[['s','Y','B'],['M','x']]).
p([['O','j','v'],['p','z','w'],['d','j','D','r']],[['O','j'],['p','z'],['d','j','D']]).
p([['C','X','k','l'],['r','q','R']],[['C','X','k'],['r','q']]).
p([['o','r','w','f'],['X','s','r','Z'],['K','D','d'],['U','S','Y']],[['o','r','w'],['X','s','r'],['K','D'],['U','S']]).
p([['h','H','Z'],['U','Q','S'],['C','u','t']],[['h','H'],['U','Q'],['C','u']]).
q([['P','W','f'],['Y','b','w']],[['P','W'],['Y','b','w']]).
q([['U','V','b','Z'],['m','C','l','n'],['A','h','D']],[['U','V','b','Z'],['m','C','l'],['A','h','D']]).
q([['n','b','C'],['Q','K','O'],['k','D','u','c'],['I','Q','u','L']],[['n','b','C'],['Q','K'],['k','D','u','c'],['I','Q','u','L']]).
q([['y','T','D'],['d','g','m'],['w','O','r','K'],['F','A','Z']],[['y','T'],['d','g'],['w','O','r','K'],['F','A','Z']]).
q([['S','A','w','S'],['M','M','E','Y']],[['S','A','w','S'],['M','M','E']]).
