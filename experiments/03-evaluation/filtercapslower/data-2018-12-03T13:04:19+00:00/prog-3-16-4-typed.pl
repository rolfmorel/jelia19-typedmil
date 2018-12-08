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

my_double4(N,M):-M is 2*N,M =< 10.
my_succ5(A,B):-succ(A,B),B =< 10.
my_msort6(A,B):-msort(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_set8(A):-list_to_set(A,A).
my_element9(A,B):-member(B,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_flatten11(A,B):-flatten(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_even14(A):-0 is A mod 2.
my_odd15(A):-1 is A mod 2.
my_reverse16(A,B):-reverse(A,B).
my_tail17([_|TL],TL).
my_max_list18(A,B):-max_list(A,B).
my_last19(A,B):-last(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_double4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_lowercase7,[char]).
prim(my_set8,[list(_)]).
prim(my_element9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_sumlist12,[list(int),int]).
prim(my_len13,[list(_),int]).
prim(my_even14,[int]).
prim(my_odd15,[int]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_tail17,[list(T),list(T)]).
prim(my_max_list18,[list(int),int]).
prim(my_last19,[list(T),T]).
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
p([k,'I','F',j,y,'I'],[i,f,i]).
p([l,'L',h,v,z,w,'I','R',q],[l,i,r]).
p(['I',x,d,'V',b,g,b,'Y'],[i,v,y]).
p([l,n,'C','D'],[c,d]).
p(['T','M','D','F','Z','M','R'],[t,m,d,f,z,m,r]).
q([c,'R','U',y,n,r,'F',c],[f,k,u,r]).
q(['I',u,'O','W',x,q],[i,i,o,w]).
q([b,'J','G',h],[g,j,j]).
q(['Q','F','L',l,'R','Y','V','L','Z'],[l,z,r,q,f,y,v,l,l]).
q(['L','Q',f,i],[q,l,'N']).
