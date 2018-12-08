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
my_lowercase5(A):-downcase_atom(A,A).
my_succ6(A,B):-succ(A,B),B =< 10.
my_flatten7(A,B):-flatten(A,B).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_succ6,[int,int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_len8,[list(_),int]).
prim(my_reverse9,[list(T),list(T)]).
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
p([s,'P',e,j,a,'T','J'],[p,t,j]).
p(['P','T','Q','Z','B',q],[p,t,q,z,b]).
p([s,'I','T','K',m,'Z'],[i,t,k,z]).
p(['N','V','B','F'],[n,v,b,f]).
p([w,'K',g,'P','K'],[k,p,k]).
q(['X','A','S','Q',p,'R',n,u,w],[q,z,a,x,s,r]).
q([o,'W','W','S','W',h,k],[w,w,'V',w,s]).
q([u,'P',h,'Z'],[p,'M',z]).
q(['M',x,p,k,'P',d],[p,a,m]).
q(['Q',e,'O',u,'I',n,'S'],[s,o,i,'C',q]).
