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
my_odd3(A):-1 is A mod 2.
my_uppercase4(A):-upcase_atom(A,A).
my_flatten5(A,B):-flatten(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_succ7(A,B):-succ(A,B),B =< 10.
my_lowercase8(A):-downcase_atom(A,A).
my_msort9(A,B):-msort(A,B).
my_even10(A):-0 is A mod 2.
my_tolower11(A,B):-downcase_atom(A,B).
my_set12(A):-list_to_set(A,A).
my_double13(N,M):-M is 2*N,M =< 10.
my_min_list14(A,B):-min_list(A,B).
my_len15(A,B):-length(A,B).
my_max_list16(A,B):-max_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
my_reverse19(A,B):-reverse(A,B).
my_list_to_set20(A,B):-list_to_set(A,B).
my_toupper21(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_odd3,[int]).
prim(my_uppercase4,[char]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_pred6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_lowercase8,[char]).
prim(my_msort9,[list(int),list(int)]).
prim(my_even10,[int]).
prim(my_tolower11,[char,char]).
prim(my_set12,[list(_)]).
prim(my_double13,[int,int]).
prim(my_min_list14,[list(int),int]).
prim(my_len15,[list(_),int]).
prim(my_max_list16,[list(int),int]).
prim(my_sumlist17,[list(int),int]).
prim(my_last18,[list(T),T]).
prim(my_reverse19,[list(T),list(T)]).
prim(my_list_to_set20,[list(T),list(T)]).
prim(my_toupper21,[char,char]).
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
p(['I','T','t','i','z','E','Q','i','P'],'i').
p(['j','x','Y','c','Y'],'Y').
p(['j','o','Y','k','k','n','Q','j','P','W'],'k').
p(['g','t','D','t','W','k'],'t').
p(['U','U','i','Q','y','A'],'U').
q(['Q','M','T','M','M','z','r','R'],'z').
q(['D','B','T','u','k','E','o','h','L','D'],'u').
q(['W','z','z','O','y','U'],'O').
q(['N','J','U','Z','V','M','o','T','g','B','Z'],'V').
q(['V','V','V','L','C','p','r','P','w'],'L').
