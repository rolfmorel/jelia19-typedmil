:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
%metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_even3(A):-0 is A mod 2.
my_pred4(A,B):-succ(B,A),A > 0.
my_set5(A):-list_to_set(A,A).
my_odd6(A):-1 is A mod 2.
my_lowercase7(A):-downcase_atom(A,A).
my_len8(A,B):-length(A,B).
my_flatten9(A,B):-flatten(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_max_list11(A,B):-max_list(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_msort13(A,B):-msort(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_tolower15(A,B):-downcase_atom(A,B).
my_reverse16(A,B):-reverse(A,B).
my_min_list17(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_even3,[int]).
prim(my_pred4,[int,int]).
prim(my_set5,[list(_)]).
prim(my_odd6,[int]).
prim(my_lowercase7,[char]).
prim(my_len8,[list(_),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_max_list11,[list(int),int]).
prim(my_succ12,[int,int]).
prim(my_msort13,[list(int),list(int)]).
prim(my_double14,[int,int]).
prim(my_tolower15,[char,char]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_min_list17,[list(int),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),char],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['H','h','C','A','I','h','s'],'h').
p(['O','x','n','n','a','m','n','a','b'],'n').
p(['y','h','J','p','Z','t','o','J','y','J'],'y').
p(['n','h','d','e','h','x','D'],'h').
p(['f','f','n','Z','K'],'f').
q(['l','l','f','o','F','P'],'F').
q(['W','N','M','v','M','a'],'a').
q(['Y','t','P','T','e','n','h','P','g','Y','p'],'h').
q(['y','E','a','y','D','H','h','h'],'a').
q(['C','x','c','W','v','Z','W','I'],'Z').
