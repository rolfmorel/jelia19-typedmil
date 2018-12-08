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
my_last3(A,B):-last(A,B).
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_len7(A,B):-length(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_set9(A):-list_to_set(A,A).
my_uppercase10(A):-upcase_atom(A,A).
my_lowercase11(A):-downcase_atom(A,A).
my_succ12(A,B):-succ(A,B),B =< 10.
my_pred13(A,B):-succ(B,A),A > 0.
my_odd14(A):-1 is A mod 2.
my_min_list15(A,B):-min_list(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_flatten17(A,B):-flatten(A,B).
my_reverse18(A,B):-reverse(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_msort4,[list(int),list(int)]).
prim(my_max_list5,[list(int),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_len7,[list(_),int]).
prim(my_double8,[int,int]).
prim(my_set9,[list(_)]).
prim(my_uppercase10,[char]).
prim(my_lowercase11,[char]).
prim(my_succ12,[int,int]).
prim(my_pred13,[int,int]).
prim(my_odd14,[int]).
prim(my_min_list15,[list(int),int]).
prim(my_sumlist16,[list(int),int]).
prim(my_flatten17,[list(list(T)),list(T)]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_toupper19,[char,char]).
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
p(['m','z','j','m','O','F','f'],'m').
p(['Q','g','r','n','F','g','n','f','D','Z'],'g').
p(['t','F','T','C','x','O','T'],'T').
p(['X','x','H','x','W','J','c','E','n'],'x').
p(['z','O','L','W','w','E','h','I','O','x'],'O').
q(['Y','j','l','Z','k','q','k'],'l').
q(['n','T','c','q','A','E','A','A','B'],'T').
q(['c','n','o','n','F','t','n'],'c').
q(['x','z','x','B','y','l','g','S','u','z'],'B').
q(['K','I','I','r','O','[','y','S','L','Y'],'[').
