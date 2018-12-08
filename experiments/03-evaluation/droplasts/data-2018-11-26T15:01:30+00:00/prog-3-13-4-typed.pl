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

my_last4(A,B):-last(A,B).
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_msort7(A,B):-msort(A,B).
my_len8(A,B):-length(A,B).
my_element9(A,B):-member(B,A).
my_max_list10(A,B):-max_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_head13([H|_],H).
my_lowercase14(A):-downcase_atom(A,A).
my_toupper15(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_last4,[list(T),T]).
prim(my_odd5,[int]).
prim(my_set6,[list(_)]).
prim(my_msort7,[list(int),list(int)]).
prim(my_len8,[list(_),int]).
prim(my_element9,[list(T),T]).
prim(my_max_list10,[list(int),int]).
prim(my_tolower11,[char,char]).
prim(my_succ12,[int,int]).
prim(my_head13,[list(T),T]).
prim(my_lowercase14,[char]).
prim(my_toupper15,[char,char]).
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
p([['f','n','c'],['A','b','D','X'],['r','g','W'],['t','W','W','C']],[['f','n'],['A','b','D'],['r','g'],['t','W','W']]).
p([['e','j','n'],['B','K','N'],['A','y','S'],['o','O','A','F']],[['e','j'],['B','K'],['A','y'],['o','O','A']]).
p([['t','X','G'],['e','t','Y']],[['t','X'],['e','t']]).
p([['F','K','t','C'],['N','s','Q','D'],['T','R','O'],['U','r','t']],[['F','K','t'],['N','s','Q'],['T','R'],['U','r']]).
p([['B','Q','W'],['t','s','m']],[['B','Q'],['t','s']]).
q([['k','o','V','z'],['j','m','T','s'],['K','b','N'],['L','L','x','p']],[['k','o','V','z'],['j','m','T'],['K','b','N'],['L','L','x']]).
q([['G','q','U','C'],['J','C','t'],['j','y','G'],['C','P','D','H']],[['G','q','U','C'],['J','C'],['j','y','G'],['C','P','D']]).
q([['k','x','X'],['F','w','u','v'],['a','C','o'],['k','z','H','z']],[['k','x','X'],['F','w','u'],['a','C','o'],['k','z','H']]).
q([['t','V','U','k'],['v','b','W'],['S','l','v','o']],[['t','V','U','k'],['v','b','W'],['S','l','v']]).
q([['w','m','w','g'],['v','x','h','a']],[['w','m','w','g'],['v','x','h']]).
