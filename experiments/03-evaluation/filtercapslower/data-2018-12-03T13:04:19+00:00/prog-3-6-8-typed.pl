:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_even4(A):-0 is A mod 2.
my_max_list5(A,B):-max_list(A,B).
my_odd6(A):-1 is A mod 2.
my_set7(A):-list_to_set(A,A).
my_flatten8(A,B):-flatten(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_even4,[int]).
prim(my_max_list5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_set7,[list(_)]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_list_to_set9,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([x,u,t,'N','U','Q','S','O','G'],[n,u,q,s,o,g]).
p(['J',m,n,g,k,a,'D'],[j,d]).
p([r,v,i,l,'G','X','Y',l,f],[g,x,y]).
p(['D',c,a,s,'P','O','X'],[d,p,o,x]).
p([c,'V','R','J','D',u,g],[v,r,j,d]).
q(['E',z,'O',b,z,i,j,c,a],[e,p,o]).
q(['F',l,r,'M',i,'Z','A',b],[m,'W',f,a,z]).
q([p,s,g,a,b],[k]).
q([s,'X','V','V','O','C','Q','E','J'],[x,j,e,o,w,v,q,c,v]).
q([a,'Q','U','V','I',m,r,'W'],[i,u,w,m,q,v]).
