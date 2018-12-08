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
my_len3(A,B):-length(A,B).
my_reverse4(A,B):-reverse(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_msort10(A,B):-msort(A,B).
my_min_list11(A,B):-min_list(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_max_list13(A,B):-max_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_double16(N,M):-M is 2*N,M =< 10.
my_last17(A,B):-last(A,B).
my_tolower18(A,B):-downcase_atom(A,B).
my_flatten19(A,B):-flatten(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_odd5,[int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_toupper7,[char,char]).
prim(my_sumlist8,[list(int),int]).
prim(my_pred9,[int,int]).
prim(my_msort10,[list(int),list(int)]).
prim(my_min_list11,[list(int),int]).
prim(my_uppercase12,[char]).
prim(my_max_list13,[list(int),int]).
prim(my_lowercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_double16,[int,int]).
prim(my_last17,[list(T),T]).
prim(my_tolower18,[char,char]).
prim(my_flatten19,[list(list(T)),list(T)]).
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
p(['t','m','t','f','H','I','s'],'t').
p(['s','F','O','h','Y','C','Y','i','j','c'],'Y').
p(['M','l','M','x','J','U'],'M').
p(['n','z','X','T','n','q','Q','j'],'n').
p(['p','p','i','V','d','X','g','s','r'],'p').
q(['p','a','d','T','w','a','K','M','I','X'],'p').
q(['g','R','S','Z','p','X','p','J','Q','U'],'X').
q(['u','q','J','C','m','z','C','l','p','J','c'],'l').
q(['g','c','K','u','v','u'],'v').
q(['E','I','z','e','x','x','X','h'],'h').
