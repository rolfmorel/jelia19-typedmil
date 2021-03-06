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

my_toupper4(A,B):-upcase_atom(A,B).
my_last5(A,B):-last(A,B).
my_msort6(A,B):-msort(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_succ8(A,B):-succ(A,B),B =< 10.
my_min_list9(A,B):-min_list(A,B).
my_even10(A):-0 is A mod 2.
my_set11(A):-list_to_set(A,A).
my_lowercase12(A):-downcase_atom(A,A).
my_max_list13(A,B):-max_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_len15(A,B):-length(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_toupper4,[char,char]).
prim(my_last5,[list(T),T]).
prim(my_msort6,[list(int),list(int)]).
prim(my_double7,[int,int]).
prim(my_succ8,[int,int]).
prim(my_min_list9,[list(int),int]).
prim(my_even10,[int]).
prim(my_set11,[list(_)]).
prim(my_lowercase12,[char]).
prim(my_max_list13,[list(int),int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_len15,[list(_),int]).
prim(my_list_to_set16,[list(T),list(T)]).
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
p(['E',u,s,t,'F',d,t],[e,f]).
p([y,v,n,z,'J','V','U','Q',l],[j,v,u,q]).
p([p,'T','N',p,'M',u,s,r,'H'],[t,n,m,h]).
p([g,d,r,r,h,'G',i,y],[g]).
p(['B','N','W','U',v,'W','O','I'],[b,n,w,u,w,o,i]).
q([w,b,m,'I',u,'F',l,p],[f,d,i]).
q([v,'U','D','D',z,f],[u,d,d,p]).
q(['L','D','B',x,'L','N','S',v,n],[l,d,l,b,'U',n,s]).
q(['X','H',l,a,k,x],[h,'E',x]).
q([j,r,'O','X',p,'C',t,q,'H'],[h,c,h,x,o]).
