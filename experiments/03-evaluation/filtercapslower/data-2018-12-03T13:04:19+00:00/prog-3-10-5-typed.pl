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

my_lowercase4(A):-downcase_atom(A,A).
my_succ5(A,B):-succ(A,B),B =< 10.
my_odd6(A):-1 is A mod 2.
my_reverse7(A,B):-reverse(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_tail9([_|TL],TL).
my_even10(A):-0 is A mod 2.
my_flatten11(A,B):-flatten(A,B).
my_msort12(A,B):-msort(A,B).
my_last13(A,B):-last(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_succ5,[int,int]).
prim(my_odd6,[int]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_sumlist8,[list(int),int]).
prim(my_tail9,[list(T),list(T)]).
prim(my_even10,[int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_msort12,[list(int),list(int)]).
prim(my_last13,[list(T),T]).
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
p(['I','K',y,'C',c],[i,k,c]).
p(['X','Y',u,f,j,'M',y,'J'],[x,y,m,j]).
p([v,i,k,w,q,m,n,'Z'],[z]).
p([c,t,'Z',k,'O','A',q],[z,o,a]).
p(['J',l,'V','J'],[j,v,j]).
q([u,'D',s,'S',v,'X',h],[d,s,'D',x]).
q(['F',s,'Z',z,'Q'],[z,f,r,q]).
q([p,'L','J',m,'F',s],[f,l,j,'E']).
q([y,'M','T',x,'R',o],['R',t,m,r]).
q([r,m,t,h,t,'G','Y'],[y,g,'U']).
