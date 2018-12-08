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

my_toupper3(A,B):-upcase_atom(A,B).
my_set4(A):-list_to_set(A,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_even6(A):-0 is A mod 2.
my_element7(A,B):-member(B,A).
my_sumlist8(A,B):-sumlist(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_head10([H|_],H).
my_lowercase11(A):-downcase_atom(A,A).
my_max_list12(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_toupper3,[char,char]).
prim(my_set4,[list(_)]).
prim(my_pred5,[int,int]).
prim(my_even6,[int]).
prim(my_element7,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_head10,[list(T),T]).
prim(my_lowercase11,[char]).
prim(my_max_list12,[list(int),int]).
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
p([['D','x','Z','R'],['o','e','A']],[['D','x','Z'],['o','e']]).
p([['a','l','H'],['d','G','f','G']],[['a','l'],['d','G','f']]).
p([['v','V','a','u'],['y','x','w','E'],['A','d','e','d'],['Z','i','X','w']],[['v','V','a'],['y','x','w'],['A','d','e'],['Z','i','X']]).
p([['S','V','Y','G'],['C','G','I','J'],['Y','X','t','b'],['s','U','p','n']],[['S','V','Y'],['C','G','I'],['Y','X','t'],['s','U','p']]).
p([['B','N','G','K'],['a','M','a'],['y','d','s']],[['B','N','G'],['a','M'],['y','d']]).
q([['U','L','A','h'],['m','m','K'],['f','X','I','E']],[['U','L','A','h'],['m','m','K'],['f','X','I']]).
q([['S','z','W','D'],['a','b','v','H'],['i','q','u'],['c','N','J']],[['S','z','W','D'],['a','b','v'],['i','q'],['c','N','J']]).
q([['d','k','v','b'],['Y','G','R','m'],['u','x','I','H'],['h','t','E']],[['d','k','v'],['Y','G','R'],['u','x','I','H'],['h','t','E']]).
q([['P','B','s','t'],['y','a','w','A'],['v','Z','a']],[['P','B','s'],['y','a','w','A'],['v','Z','a']]).
q([['s','D','A','D'],['i','G','q']],[['s','D','A'],['i','G','q']]).
