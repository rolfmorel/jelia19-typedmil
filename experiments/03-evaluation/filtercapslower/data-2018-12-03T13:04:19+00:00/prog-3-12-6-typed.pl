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
my_lowercase5(A):-downcase_atom(A,A).
my_odd6(A):-1 is A mod 2.
my_sumlist7(A,B):-sumlist(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_min_list9(A,B):-min_list(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_msort11(A,B):-msort(A,B).
my_element12(A,B):-member(B,A).
my_max_list13(A,B):-max_list(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_head15([H|_],H).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_even4,[int]).
prim(my_lowercase5,[char]).
prim(my_odd6,[int]).
prim(my_sumlist7,[list(int),int]).
prim(my_toupper8,[char,char]).
prim(my_min_list9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_element12,[list(T),T]).
prim(my_max_list13,[list(int),int]).
prim(my_succ14,[int,int]).
prim(my_head15,[list(T),T]).
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
p(['B','S',u,'F',a,t,g,'I'],[b,s,f,i]).
p(['S',l,m,'A','B','C',i,'N'],[s,a,b,c,n]).
p(['S','E','V','M','V'],[s,e,v,m,v]).
p([c,'P',a,'F',n],[p,f]).
p([c,a,'S','P','S'],[s,p,s]).
q([x,'J','O','A','Q',u,'J'],[o,'M',j,a,q,j]).
q([b,y,'R',w,'A',q,o],[r,g,a]).
q([f,r,'F','R','A','K',i,'B'],[r,a,b,k,w,f]).
q([e,l,'G',z,e,'D'],[e,d,g]).
q([n,e,'V',d,f,'H',j],[w,h,v]).
