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
my_toupper3(A,B):-upcase_atom(A,B).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_succ8(A,B):-succ(A,B),B =< 10.
my_len9(A,B):-length(A,B).
my_flatten10(A,B):-flatten(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_msort12(A,B):-msort(A,B).
my_set13(A):-list_to_set(A,A).
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_last19(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_max_list4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_lowercase7,[char]).
prim(my_succ8,[int,int]).
prim(my_len9,[list(_),int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_double11,[int,int]).
prim(my_msort12,[list(int),list(int)]).
prim(my_set13,[list(_)]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_tolower15,[char,char]).
prim(my_sumlist16,[list(int),int]).
prim(my_uppercase17,[char]).
prim(my_pred18,[int,int]).
prim(my_last19,[list(T),T]).
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
p(['Z','M','V','b','x','x','l','D'],'x').
p(['r','Q','Q','u','R','D'],'Q').
p(['o','j','S','t','o','o','b','o','R','y'],'o').
p(['L','d','L','d','R'],'d').
p(['A','W','p','p','C','z','y','u','e','H'],'p').
q(['M','m','r','X','k','E','{','r'],'{').
q(['j','C','T','H','X','c','a','b','J','E','C'],'H').
q(['x','E','Z','I','Z','n','v'],'n').
q(['p','O','E','R','M','V','C','O','r','F','k'],'F').
q(['V','R','b','h','b','q'],'h').
