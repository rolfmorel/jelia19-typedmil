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

my_max_list4(A,B):-max_list(A,B).
my_reverse5(A,B):-reverse(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_last7(A,B):-last(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_double9(N,M):-M is 2*N,M =< 10.
my_len10(A,B):-length(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_set12(A):-list_to_set(A,A).
my_even13(A):-0 is A mod 2.
my_min_list14(A,B):-min_list(A,B).
my_element15(A,B):-member(B,A).
my_msort16(A,B):-msort(A,B).
my_lowercase17(A):-downcase_atom(A,A).
my_toupper18(A,B):-upcase_atom(A,B).
my_sumlist19(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_max_list4,[list(int),int]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_last7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_double9,[int,int]).
prim(my_len10,[list(_),int]).
prim(my_pred11,[int,int]).
prim(my_set12,[list(_)]).
prim(my_even13,[int]).
prim(my_min_list14,[list(int),int]).
prim(my_element15,[list(T),T]).
prim(my_msort16,[list(int),list(int)]).
prim(my_lowercase17,[char]).
prim(my_toupper18,[char,char]).
prim(my_sumlist19,[list(int),int]).
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
p([o,l,'F','U','M',b,'T',w,j],[f,u,m,t]).
p([m,l,'U',v,'O'],[u,o]).
p(['I','D',l,o,'R',a],[i,d,r]).
p([j,'F','H','J'],[f,h,j]).
p(['P',u,'H',l,'A','X',c,o],[p,h,a,x]).
q(['V',s,'U',e,l,'G','A'],['M',g,v,u,a]).
q(['T',v,'Q',n,'B','N',x,'G'],[t,q,b,'K',g,n]).
q([s,'R','Z',t,'Q'],[z,'C',q,r]).
q(['X',h,'G','I',a,n,'B'],[i,g,x,u,b]).
q(['P','W','Q',j,b,'E','M',j,'L'],[p,e,g,l,q,m,w]).
