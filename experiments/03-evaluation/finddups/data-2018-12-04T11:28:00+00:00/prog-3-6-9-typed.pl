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
my_pred3(A,B):-succ(B,A),A > 0.
my_flatten4(A,B):-flatten(A,B).
my_max_list5(A,B):-max_list(A,B).
my_msort6(A,B):-msort(A,B).
my_len7(A,B):-length(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_max_list5,[list(int),int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_len7,[list(_),int]).
prim(my_toupper8,[char,char]).
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
p(['U','O','l','T','T','q'],'T').
p(['u','i','i','u','i','j','B','c','d'],'i').
p(['y','x','h','G','r','h','T','z','s'],'h').
p(['S','J','h','i','g','g'],'g').
p(['W','W','v','Q','u','g','Z'],'W').
q(['d','s','h','V','H','d','t','t','w'],'w').
q(['b','s','y','I','u','y','T','Z','W','b'],'I').
q(['f','f','Y','z','i','O','f','A','i'],'Y').
q(['b','A','l','{','l','e'],'{').
q(['X','P','X','q','S','z','q'],'z').
