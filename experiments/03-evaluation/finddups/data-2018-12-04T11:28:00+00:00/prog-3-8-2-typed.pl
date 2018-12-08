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
my_odd4(A):-1 is A mod 2.
my_sumlist5(A,B):-sumlist(A,B).
my_msort6(A,B):-msort(A,B).
my_uppercase7(A):-upcase_atom(A,A).
my_pred8(A,B):-succ(B,A),A > 0.
my_list_to_set9(A,B):-list_to_set(A,B).
my_even10(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_odd4,[int]).
prim(my_sumlist5,[list(int),int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_uppercase7,[char]).
prim(my_pred8,[int,int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_even10,[int]).
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
p(['x','c','G','y','y','U','l'],'y').
p(['c','D','R','S','A','R','P','k','k'],'k').
p(['O','X','O','f','w','u','W'],'O').
p(['p','U','D','I','f','p'],'p').
p(['j','B','j','p','C','V','i','l','N','f'],'j').
q(['T','J','e','w','J','J','o','Z'],'Z').
q(['z','f','P','f','Y','R','G','Y','w','v'],'R').
q(['y','Y','P','a','O','C','P'],'a').
q(['G','q','G','W','q','l','u','H','w','h','L'],'u').
q(['g','H','f','a','l','g'],'f').
