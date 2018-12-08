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

my_head3([H|_],H).

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

my_odd5(A):-1 is A mod 2.
my_toupper6(A,B):-upcase_atom(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_set8(A):-list_to_set(A,A).
my_len9(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_head3,[list(T),T]).
prim(my_odd5,[int]).
prim(my_toupper6,[char,char]).
prim(my_tolower7,[char,char]).
prim(my_set8,[list(_)]).
prim(my_len9,[list(_),int]).
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
p([['P','d','k','J'],['S','V','R'],['K','q','z','c']],[['P','d','k'],['S','V'],['K','q','z']]).
p([['p','b','H'],['K','U','m','q'],['G','v','p'],['i','L','p']],[['p','b'],['K','U','m'],['G','v'],['i','L']]).
p([['B','z','m','h'],['V','u','x'],['b','t','L']],[['B','z','m'],['V','u'],['b','t']]).
p([['v','G','o','F'],['o','o','H'],['t','y','p']],[['v','G','o'],['o','o'],['t','y']]).
p([['V','Y','u'],['f','i','O','b'],['I','q','w'],['v','N','g','X']],[['V','Y'],['f','i','O'],['I','q'],['v','N','g']]).
q([['z','X','o','Z'],['I','b','u','X'],['i','G','k','Z']],[['z','X','o','Z'],['I','b','u','X'],['i','G','k']]).
q([['y','v','e','y'],['q','m','y'],['s','W','G'],['G','c','j','g']],[['y','v','e','y'],['q','m'],['s','W'],['G','c','j','g']]).
q([['H','B','i'],['b','V','l','Z']],[['H','B','i'],['b','V','l']]).
q([['l','k','x','c'],['b','l','E']],[['l','k','x','c'],['b','l']]).
q([['U','X','i','a'],['v','b','U']],[['U','X','i','a'],['v','b']]).
