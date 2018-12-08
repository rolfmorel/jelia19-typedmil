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
my_lowercase3(A):-downcase_atom(A,A).
my_set4(A):-list_to_set(A,A).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_max_list8(A,B):-max_list(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_even12(A):-0 is A mod 2.
my_odd13(A):-1 is A mod 2.
my_reverse14(A,B):-reverse(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_pred16(A,B):-succ(B,A),A > 0.
my_list_to_set17(A,B):-list_to_set(A,B).
my_msort18(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_lowercase3,[char]).
prim(my_set4,[list(_)]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_toupper7,[char,char]).
prim(my_max_list8,[list(int),int]).
prim(my_double9,[int,int]).
prim(my_last10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_even12,[int]).
prim(my_odd13,[int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_succ15,[int,int]).
prim(my_pred16,[int,int]).
prim(my_list_to_set17,[list(T),list(T)]).
prim(my_msort18,[list(int),list(int)]).
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
p(['B','h','q','H','h','g'],'h').
p(['C','G','H','v','Z','C'],'C').
p(['S','v','z','Y','M','Y','Z','g'],'Y').
p(['c','g','I','b','w','P','I','m'],'I').
p(['C','O','i','E','R','V','t','R','t'],'t').
q(['Y','s','Y','u','[','u','U'],'[').
q(['h','q','C','D','C','s','V','P'],'V').
q(['I','K','L','G','Q','G','w','M'],'I').
q(['U','O','c','W','h','W','w','j'],'O').
q(['l','I','l','r','y','y','n','G','O'],'O').
