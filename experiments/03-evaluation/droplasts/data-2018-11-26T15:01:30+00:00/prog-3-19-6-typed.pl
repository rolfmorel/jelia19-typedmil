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

my_uppercase3(A):-upcase_atom(A,A).
my_tolower4(A,B):-downcase_atom(A,B).
my_max_list5(A,B):-max_list(A,B).

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

my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
my_min_list9(A,B):-min_list(A,B).
my_flatten10(A,B):-flatten(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_odd12(A):-1 is A mod 2.
my_succ13(A,B):-succ(A,B),B =< 10.
my_toupper14(A,B):-upcase_atom(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_list_to_set16(A,B):-list_to_set(A,B).
my_even17(A):-0 is A mod 2.
my_sumlist18(A,B):-sumlist(A,B).
my_msort19(A,B):-msort(A,B).
my_double20(N,M):-M is 2*N,M =< 10.
my_head21([H|_],H).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_tolower4,[char,char]).
prim(my_max_list5,[list(int),int]).
prim(my_set7,[list(_)]).
prim(my_len8,[list(_),int]).
prim(my_min_list9,[list(int),int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_lowercase11,[char]).
prim(my_odd12,[int]).
prim(my_succ13,[int,int]).
prim(my_toupper14,[char,char]).
prim(my_pred15,[int,int]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_even17,[int]).
prim(my_sumlist18,[list(int),int]).
prim(my_msort19,[list(int),list(int)]).
prim(my_double20,[int,int]).
prim(my_head21,[list(T),T]).
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
p([['D','U','J'],['K','c','v'],['U','d','R'],['g','e','e','y']],[['D','U'],['K','c'],['U','d'],['g','e','e']]).
p([['f','G','D'],['o','R','A','v'],['I','w','d']],[['f','G'],['o','R','A'],['I','w']]).
p([['r','Q','U','w'],['i','i','y','y'],['R','P','x'],['q','o','g','b']],[['r','Q','U'],['i','i','y'],['R','P'],['q','o','g']]).
p([['q','j','w'],['M','o','i'],['M','e','O'],['R','D','T']],[['q','j'],['M','o'],['M','e'],['R','D']]).
p([['y','i','k'],['D','w','l','L']],[['y','i'],['D','w','l']]).
q([['f','p','y'],['r','t','T'],['e','N','A'],['C','f','r']],[['f','p'],['r','t','T'],['e','N','A'],['C','f','r']]).
q([['l','x','L','a'],['a','N','T'],['Y','a','M']],[['l','x','L'],['a','N','T'],['Y','a','M']]).
q([['i','a','G','g'],['j','h','z']],[['i','a','G','g'],['j','h']]).
q([['S','m','m','H'],['d','G','R'],['I','R','v'],['K','w','v','G']],[['S','m','m'],['d','G','R'],['I','R'],['K','w','v','G']]).
q([['E','Y','V'],['z','I','V','a']],[['E','Y','V'],['z','I','V']]).
