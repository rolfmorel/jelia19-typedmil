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

my_reverse4(A,B):-reverse(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_element8(A,B):-member(B,A).
my_set9(A):-list_to_set(A,A).
my_double10(N,M):-M is 2*N,M =< 10.
my_flatten11(A,B):-flatten(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_max_list13(A,B):-max_list(A,B).
my_min_list14(A,B):-min_list(A,B).
my_msort15(A,B):-msort(A,B).
my_even16(A):-0 is A mod 2.
my_len17(A,B):-length(A,B).
my_tail18([_|TL],TL).
my_head19([H|_],H).
my_last20(A,B):-last(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_toupper5,[char,char]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_sumlist7,[list(int),int]).
prim(my_element8,[list(T),T]).
prim(my_set9,[list(_)]).
prim(my_double10,[int,int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_succ12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_min_list14,[list(int),int]).
prim(my_msort15,[list(int),list(int)]).
prim(my_even16,[int]).
prim(my_len17,[list(_),int]).
prim(my_tail18,[list(T),list(T)]).
prim(my_head19,[list(T),T]).
prim(my_last20,[list(T),T]).
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
p(['C','S','Z','L','V','G',p,n,x],[c,s,z,l,v,g]).
p(['W','A','H',e,'R','N'],[w,a,h,r,n]).
p([p,m,f,y,r,r,'B','C',u],[b,c]).
p(['R',t,'U','F',m,a,r,'P',r],[r,u,f,p]).
p(['P','U','K',w,s,a,o,'P',f],[p,u,k,p]).
q([b,'U','X',q,h],[u,x,'B']).
q([d,'Q',u,'D',f,f,z,'I','N'],['W',d,i,n,q]).
q([d,'P','N',v,'L','Q','Y'],[e,y,p,l,n,q]).
q([b,'W','K',p,m,'H'],[e,w,h,k]).
q([k,'C','X',o,'T',i,'T'],[b,t,x,t,c]).
