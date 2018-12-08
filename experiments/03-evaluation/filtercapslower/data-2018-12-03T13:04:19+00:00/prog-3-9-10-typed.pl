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

my_last4(A,B):-last(A,B).
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_succ7(A,B):-succ(A,B),B =< 10.
my_msort8(A,B):-msort(A,B).
my_reverse9(A,B):-reverse(A,B).
my_tail10([_|TL],TL).
my_flatten11(A,B):-flatten(A,B).
my_element12(A,B):-member(B,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_odd5,[int]).
prim(my_set6,[list(_)]).
prim(my_succ7,[int,int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_reverse9,[list(T),list(T)]).
prim(my_tail10,[list(T),list(T)]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_element12,[list(T),T]).
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
p([u,q,'W',r,'P','N'],[w,p,n]).
p(['P','T','V','X','V','G','F',d,'B'],[p,t,v,x,v,g,f,b]).
p(['P',q,'Q',f,'G',t,'I',p],[p,q,g,i]).
p([r,f,o,'E','G',u,'C',h],[e,g,c]).
p([g,'I',t,'Y'],[i,y]).
q([m,j,l,'M','S',g,m,e],['B',m,s]).
q([y,'Z',o,p,'R','I',g],[i,'F',r,z]).
q([t,'T','O','S',g,l,'S','U','D'],[o,t,s,s,d,m,u]).
q(['S',d,'I','P','X'],['M',i,p,x,s]).
q([a,'S',c,o,j,'E',d,k,'Z'],[s,z,e,'Y']).
