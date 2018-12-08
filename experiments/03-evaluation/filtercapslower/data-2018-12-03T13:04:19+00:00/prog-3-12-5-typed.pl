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

my_set4(A):-list_to_set(A,A).
my_element5(A,B):-member(B,A).
my_odd6(A):-1 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_tail8([_|TL],TL).
my_even9(A):-0 is A mod 2.
my_succ10(A,B):-succ(A,B),B =< 10.
my_msort11(A,B):-msort(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_sumlist15(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_set4,[list(_)]).
prim(my_element5,[list(T),T]).
prim(my_odd6,[int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_tail8,[list(T),list(T)]).
prim(my_even9,[int]).
prim(my_succ10,[int,int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_double12,[int,int]).
prim(my_min_list13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_sumlist15,[list(int),int]).
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
p(['W','X',e,f],[w,x]).
p([v,r,'T','I'],[t,i]).
p(['U',a,'S',z,v,'O',m,'R','O'],[u,s,o,r,o]).
p([n,u,'I',k],[i]).
p(['K','K',m,n,'A',a,'A','C',m],[k,k,a,a,c]).
q(['N',k,z,'A','T','L','E','J',m],[n,l,t,a,j,e,m]).
q([q,'V','F',f,x],['F',f,v]).
q([i,'G',h,g,j,'F',a,'R'],[g,r,f,c]).
q(['P',q,'Q','O',u],[o,m,q,p]).
q(['A',q,'P',p,'P','G','T','M'],[t,p,g,a,p,'S',m]).
