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
my_set5(A):-list_to_set(A,A).
my_odd6(A):-1 is A mod 2.
my_even7(A):-0 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_min_list10(A,B):-min_list(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
my_msort16(A,B):-msort(A,B).
my_head17([H|_],H).
my_lowercase18(A):-downcase_atom(A,A).
my_list_to_set19(A,B):-list_to_set(A,B).
my_element20(A,B):-member(B,A).
my_reverse21(A,B):-reverse(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_set5,[list(_)]).
prim(my_odd6,[int]).
prim(my_even7,[int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_double9,[int,int]).
prim(my_min_list10,[list(int),int]).
prim(my_tail11,[list(T),list(T)]).
prim(my_sumlist12,[list(int),int]).
prim(my_toupper13,[char,char]).
prim(my_max_list14,[list(int),int]).
prim(my_len15,[list(_),int]).
prim(my_msort16,[list(int),list(int)]).
prim(my_head17,[list(T),T]).
prim(my_lowercase18,[char]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_element20,[list(T),T]).
prim(my_reverse21,[list(T),list(T)]).
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
p([h,q,i,g,'N','L'],[n,l]).
p(['P','O','O',w],[p,o,o]).
p([l,'R',m,'Q',y,'E','R'],[r,q,e,r]).
p([s,c,'C','X','K','F'],[c,x,k,f]).
p([l,'S','B','Z','F',j],[s,b,z,f]).
q(['F','E',z,w,v,y],[f,t,e]).
q(['Y','Q','J','R','R',b,'N'],[u,y,n,r,q,j,r]).
q([m,'E',z,'A','D',k,b,w],[a,d,v,e]).
q(['L','E',u,r,'K',t,i],[l,k,e,'B']).
q([n,n,n,k,g,'C',m,f,'A'],[c,a,i]).
