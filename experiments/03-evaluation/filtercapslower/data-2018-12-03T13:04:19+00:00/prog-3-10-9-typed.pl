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

my_pred4(A,B):-succ(B,A),A > 0.
my_succ5(A,B):-succ(A,B),B =< 10.
my_tail6([_|TL],TL).
my_len7(A,B):-length(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_tail6,[list(T),list(T)]).
prim(my_len7,[list(_),int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_toupper9,[char,char]).
prim(my_min_list10,[list(int),int]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_head12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
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
p(['G','V','G',x,'J','E',n],[g,v,g,j,e]).
p(['K',g,b,u],[k]).
p(['N',p,'H',q,c,'L','J','Q',u],[n,h,l,j,q]).
p([v,'U','Z','B','J',l],[u,z,b,j]).
p([d,z,u,d,e],[]).
q([e,j,'E','B','Z',y,'E',p,j],[d,z,e,e,b]).
q([m,y,'E','E',m],['Y',e,e]).
q(['Q',h,p,'M','D','T'],[m,'N',q,d,t]).
q(['A',v,m,'I',c,'E'],[i,'F',e,a]).
q(['G','W',x,'L','I','R',t,d],[r,f,w,l,i,g]).
