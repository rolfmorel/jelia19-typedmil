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
my_msort3(A,B):-msort(A,B).
my_last4(A,B):-last(A,B).
my_set5(A):-list_to_set(A,A).
my_list_to_set6(A,B):-list_to_set(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_max_list8(A,B):-max_list(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_min_list10(A,B):-min_list(A,B).
my_flatten11(A,B):-flatten(A,B).
my_reverse12(A,B):-reverse(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_double14(N,M):-M is 2*N,M =< 10.
my_even15(A):-0 is A mod 2.
my_sumlist16(A,B):-sumlist(A,B).
my_len17(A,B):-length(A,B).
my_toupper18(A,B):-upcase_atom(A,B).
my_pred19(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_msort3,[list(int),list(int)]).
prim(my_last4,[list(T),T]).
prim(my_set5,[list(_)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_succ7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_lowercase9,[char]).
prim(my_min_list10,[list(int),int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_uppercase13,[char]).
prim(my_double14,[int,int]).
prim(my_even15,[int]).
prim(my_sumlist16,[list(int),int]).
prim(my_len17,[list(_),int]).
prim(my_toupper18,[char,char]).
prim(my_pred19,[int,int]).
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
p(['r','g','g','x','k','c','q'],'g').
p(['y','X','K','x','f','H','f','o'],'f').
p(['j','D','h','T','v','v'],'v').
p(['d','t','d','z','N','H','n'],'d').
p(['o','r','v','h','e','G','e','S','Q','h'],'h').
q(['L','W','H','T','T','Y','x','s','D','U','z'],'H').
q(['n','u','n','Q','L','Y','d'],'u').
q(['F','Y','g','c','P','c'],'Y').
q(['C','N','s','s','x','C','W'],'N').
q(['j','G','M','e','j','j','L','L'],'M').
