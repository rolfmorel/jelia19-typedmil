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
my_reverse3(A,B):-reverse(A,B).
my_flatten4(A,B):-flatten(A,B).
my_last5(A,B):-last(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_tolower7(A,B):-downcase_atom(A,B).
my_odd8(A):-1 is A mod 2.
my_len9(A,B):-length(A,B).
my_set10(A):-list_to_set(A,A).
my_sumlist11(A,B):-sumlist(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_toupper13(A,B):-upcase_atom(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_succ15(A,B):-succ(A,B),B =< 10.
my_list_to_set16(A,B):-list_to_set(A,B).
my_msort17(A,B):-msort(A,B).
my_uppercase18(A):-upcase_atom(A,A).
my_max_list19(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_last5,[list(T),T]).
prim(my_double6,[int,int]).
prim(my_tolower7,[char,char]).
prim(my_odd8,[int]).
prim(my_len9,[list(_),int]).
prim(my_set10,[list(_)]).
prim(my_sumlist11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_toupper13,[char,char]).
prim(my_lowercase14,[char]).
prim(my_succ15,[int,int]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_msort17,[list(int),list(int)]).
prim(my_uppercase18,[char]).
prim(my_max_list19,[list(int),int]).
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
p(['i','i','P','w','j','Q','P','o','V','G'],'i').
p(['r','N','P','G','y','A','V','w','N','j'],'N').
p(['P','E','d','p','x','z','l','s','o','l'],'l').
p(['p','J','d','W','W','w','p','I','g'],'W').
p(['h','S','m','m','E','x','J','x','K','t'],'m').
q(['W','t','k','I','J','W','o'],'t').
q(['U','m','c','S','g','B','q','Z','v','y','S'],'q').
q(['I','W','J','B','z','M','W','F','Q','Y'],'M').
q(['q','L','z','j','R','L','V','G','n','b','K'],'b').
q(['j','t','C','J','S','K','w','C'],'j').
