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
my_double6(N,M):-M is 2*N,M =< 10.
my_flatten7(A,B):-flatten(A,B).
my_element8(A,B):-member(B,A).
my_odd9(A):-1 is A mod 2.
my_toupper10(A,B):-upcase_atom(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_max_list14(A,B):-max_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_double6,[int,int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_element8,[list(T),T]).
prim(my_odd9,[int]).
prim(my_toupper10,[char,char]).
prim(my_sumlist11,[list(int),int]).
prim(my_len12,[list(_),int]).
prim(my_pred13,[int,int]).
prim(my_max_list14,[list(int),int]).
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
p(['C',m,d,v,'A','I',k],[c,a,i]).
p(['M',i,q,p,q,i],[m]).
p(['R','P',g,'Q',b,'S'],[r,p,q,s]).
p(['X','T',q,'R'],[x,t,r]).
p([e,m,'U','L',f,'C','N'],[u,l,c,n]).
q([i,'M','D',r,m,'N','I','S'],[n,s,d,i,d,m]).
q([e,t,'A',p,j,'H'],['Z',a,h]).
q(['S','K',j,q,'L'],[l,s,k,'O']).
q([c,j,h,h,'M',u,'N'],[m,y,n]).
q(['R',i,g,c],[n,r]).
