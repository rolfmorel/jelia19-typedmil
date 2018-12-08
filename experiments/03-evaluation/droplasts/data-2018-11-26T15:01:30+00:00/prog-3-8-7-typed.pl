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

my_max_list3(A,B):-max_list(A,B).
my_toupper4(A,B):-upcase_atom(A,B).
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_even7(A):-0 is A mod 2.
my_min_list8(A,B):-min_list(A,B).
my_element9(A,B):-member(B,A).
my_uppercase10(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_toupper4,[char,char]).
prim(my_len5,[list(_),int]).
prim(my_set6,[list(_)]).
prim(my_even7,[int]).
prim(my_min_list8,[list(int),int]).
prim(my_element9,[list(T),T]).
prim(my_uppercase10,[char]).
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
p([['G','P','w','I'],['V','T','K','T']],[['G','P','w'],['V','T','K']]).
p([['v','g','H','c'],['w','D','S'],['F','V','N','N']],[['v','g','H'],['w','D'],['F','V','N']]).
p([['I','E','c'],['C','O','j'],['i','d','c']],[['I','E'],['C','O'],['i','d']]).
p([['J','x','o','Q'],['Y','g','Z'],['U','F','Y','U']],[['J','x','o'],['Y','g'],['U','F','Y']]).
p([['W','O','Y'],['u','I','j'],['E','p','A','Z'],['p','M','j']],[['W','O'],['u','I'],['E','p','A'],['p','M']]).
q([['J','G','v','I'],['S','B','p','M']],[['J','G','v','I'],['S','B','p']]).
q([['N','t','s'],['i','L','C']],[['N','t'],['i','L','C']]).
q([['S','V','i'],['G','S','r','w'],['u','c','M','z'],['v','K','P','L']],[['S','V','i'],['G','S','r','w'],['u','c','M'],['v','K','P','L']]).
q([['j','F','v','r'],['f','b','c'],['r','q','T']],[['j','F','v','r'],['f','b','c'],['r','q']]).
q([['y','w','D','x'],['h','f','m'],['z','u','o','d'],['E','p','z']],[['y','w','D','x'],['h','f'],['z','u','o'],['E','p','z']]).
