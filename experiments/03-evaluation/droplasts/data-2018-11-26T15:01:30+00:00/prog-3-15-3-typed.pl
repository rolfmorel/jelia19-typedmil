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
my_sumlist4(A,B):-sumlist(A,B).
my_odd5(A):-1 is A mod 2.
my_toupper6(A,B):-upcase_atom(A,B).

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

my_element8(A,B):-member(B,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_tolower11(A,B):-downcase_atom(A,B).
my_head12([H|_],H).
my_uppercase13(A):-upcase_atom(A,A).
my_min_list14(A,B):-min_list(A,B).
my_flatten15(A,B):-flatten(A,B).
my_set16(A):-list_to_set(A,A).
my_len17(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_sumlist4,[list(int),int]).
prim(my_odd5,[int]).
prim(my_toupper6,[char,char]).
prim(my_element8,[list(T),T]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_lowercase10,[char]).
prim(my_tolower11,[char,char]).
prim(my_head12,[list(T),T]).
prim(my_uppercase13,[char]).
prim(my_min_list14,[list(int),int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_set16,[list(_)]).
prim(my_len17,[list(_),int]).
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
p([['o','f','O'],['c','R','y','V']],[['o','f'],['c','R','y']]).
p([['p','b','f','e'],['D','A','d']],[['p','b','f'],['D','A']]).
p([['c','d','z','N'],['S','Y','l','b']],[['c','d','z'],['S','Y','l']]).
p([['F','Z','z','T'],['N','u','L','f'],['p','D','B','O']],[['F','Z','z'],['N','u','L'],['p','D','B']]).
p([['w','E','u'],['D','Y','j','K']],[['w','E'],['D','Y','j']]).
q([['d','P','N','h'],['i','m','m','e']],[['d','P','N','h'],['i','m','m']]).
q([['w','O','e'],['e','r','K','F']],[['w','O','e'],['e','r','K']]).
q([['A','t','i'],['e','Y','B','g'],['A','j','B'],['a','f','v']],[['A','t'],['e','Y','B','g'],['A','j'],['a','f','v']]).
q([['i','K','T','z'],['G','o','Q']],[['i','K','T','z'],['G','o']]).
q([['u','R','o'],['Q','D','F']],[['u','R','o'],['Q','D']]).
