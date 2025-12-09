sets
i index for machines /1*2/
j index for jobs /1*3/
k index for position /1*3/
s index for processing speeds /1*3/
iter number of iteration /1*10/;
alias (i,h);
parameters
w weght of obj /1.1/
v(s) processing speed factor for fast and normal and slow speeds respectively /1 1.2,2 1,3 0.8/
landa(s) conversion factor for processing speed /1 1.5,2 1,3 0.6/
fi conversion factor for idle time on each machine /0.5/
pm power of each machine /60/;
table p1(i,j) processing time of job j on machine i by scenario1
         1             2             3
1        9.3185        12.2990       10.8539
2        10.3680       15.8990       7.5493;
table p2(i,j)
         1             2             3
1        18.9413       8.1047        11.5278
2        18.6162       11.7279       13.9685;
table p3(i,j)
         1             2             3
1        21.4839       20.0044       20.2184
2        19.2301       20.6727       20.1254;
positive variables
teta(i) idle time on machine i
c(i,k) completion time of kth job on machine i
st(i,k) start time of kth job on machine i
cmax the makespan
tec total energy consumption
binary variable
x(i,j,k,s) 1 if job j is the kth job which processed on machine i with speed s
variables
z11 value of first objective function by scenario 1
z12 value of second objective function by scenario 1
z21 value of first objective function by scenario 2
z22 value of second objective function by scenario 2
z31 value of first objective function by scenario 3
z32 value of second objective function by scenario 3
z1 total obj 1
z2 total obj 2
z3 total obj 3;
equations
objtotal1 total obg for 1st scenario
objcmax1 objective for minimizing makespan first scenario
objtec1 objective for minimizing total energy consumption
posal1(i,j) alocation of positions to each job
jobal1(i,k) alocation of jobs to each position
spdass1(i,j,k,h) assignment of speeds: each job on each machine processed with one speed
c111 completion time of first job on first machine
ck1(i,k) completion time of kth job on first machine: position index start with 2
ci1(i,k) completion time of first job on ith machine: machine index start with 2
cs1(i,k) ensure that jobs processed without interruption
mkspn1 calculate makespan
idl1(i) calculate idle time of each machine
enrg1 calculate energy consumption

objtotal2 total obj for 2nd senario
objcmax2 objective for minimizing makespan second scenario
objtec2 objective for minimizing total energy consumption
posal2(i,j) alocation of positions to each job
jobal2(i,k) alocation of jobs to each position
spdass2(i,j,k,h) assignment of speeds: each job on each machine processed with one speed
c112 completion time of first job on first machine
ck2(i,k) completion time of kth job on first machine: position index start with 2
ci2(i,k) completion time of first job on ith machine: machine index start with 2
cs2(i,k) ensure that jobs processed without interruption
mkspn2 calculate makespan
idl2(i) calculate idle time of each machine
enrg2 calculate energy consumption

objtotal3 total obj for 3th scenario
objcmax3 objective for minimizing makespan third scenario
objtec3 objective for minimizing total energy consumption
posal3(i,j) alocation of positions to each job
jobal3(i,k) alocation of jobs to each position
spdass3(i,j,k,h) assignment of speeds: each job on each machine processed with one speed
c113 completion time of first job on first machine
ck3(i,k) completion time of kth job on first machine: position index start with 2
ci3(i,k) completion time of first job on ith machine: machine index start with 2
cs3(i,k) ensure that jobs processed without interruption
mkspn3 calculate makespan
idl3(i) calculate idle time of each machine
enrg3 calculate energy consumption;

objtotal1..z1=e=w*z11+(1-w)*z12;
objcmax1..z11=e=cmax;
objtec1..z12=e=tec;
posal1(i,j)..sum((k,s),x(i,j,k,s))=e=1;
jobal1(i,k)..sum((j,s),x(i,j,k,s))=e=1;
spdass1(i,j,k,h)$(ord(i) ne ord(h))..sum(s,x(i,j,k,s))=e=sum(s,x(h,j,k,s));
c111..c("1","1")=g=sum((j,s),(p1("1",j)/v(s))*x("1",j,"1",s));
ck1(i,k)$(ord(k)ne 1)..c(i,k)=g=c(i,k-1)+sum((j,s),(p1(i,j)/v(s))*x(i,j,k,s));
ci1(i,k)$(ord(i)ne 1)..c(i,k)=g=c(i-1,k)+sum((j,s),(p1(i,j)/v(s))*x(i,j,k,s));
cs1(i,k)..c(i,k)=e=st(i,k)+sum((j,s),(p1(i,j)/v(s))*x(i,j,k,s));
mkspn1..cmax=g=c("2","3");
idl1(i)..teta(i)=e=cmax-sum((j,k,s),(p1(i,j)/v(s))*x(i,j,k,s));
enrg1..tec=e=sum((i,j,k,s),(pm*p1(i,j)*x(i,j,k,s)*landa(s))/(60*v(s)))+sum(i,(fi*pm*teta(i))/60);

objtotal2..z2=e=w*z21+(1-w)*z22;
objcmax2..z21=e=cmax;
objtec2..z22=e=tec;
posal2(i,j)..sum((k,s),x(i,j,k,s))=e=1;
jobal2(i,k)..sum((j,s),x(i,j,k,s))=e=1;
spdass2(i,j,k,h)$(ord(i) ne ord(h))..sum(s,x(i,j,k,s))=e=sum(s,x(h,j,k,s));
c112..c("1","1")=g=sum((j,s),(p2("1",j)/v(s))*x("1",j,"1",s));
ck2(i,k)$(ord(k)ne 1)..c(i,k)=g=c(i,k-1)+sum((j,s),(p2(i,j)/v(s))*x(i,j,k,s));
ci2(i,k)$(ord(i)ne 1)..c(i,k)=g=c(i-1,k)+sum((j,s),(p2(i,j)/v(s))*x(i,j,k,s));
cs2(i,k)..c(i,k)=e=st(i,k)+sum((j,s),(p2(i,j)/v(s))*x(i,j,k,s));
mkspn2..cmax=g=c("2","3");
idl2(i)..teta(i)=e=cmax-sum((j,k,s),(p2(i,j)/v(s))*x(i,j,k,s));
enrg2..tec=e=sum((i,j,k,s),(pm*p2(i,j)*x(i,j,k,s)*landa(s))/(60*v(s)))+sum(i,(fi*pm*teta(i))/60);

objtotal3..z3=e=w*z31+(1-w)*z32;
objcmax3..z31=e=cmax;
objtec3..z32=e=tec;
posal3(i,j)..sum((k,s),x(i,j,k,s))=e=1;
jobal3(i,k)..sum((j,s),x(i,j,k,s))=e=1;
spdass3(i,j,k,h)$(ord(i) ne ord(h))..sum(s,x(i,j,k,s))=e=sum(s,x(h,j,k,s));
c113..c("1","1")=g=sum((j,s),(p3("1",j)/v(s))*x("1",j,"1",s));
ck3(i,k)$(ord(k)ne 1)..c(i,k)=g=c(i,k-1)+sum((j,s),(p3(i,j)/v(s))*x(i,j,k,s));
ci3(i,k)$(ord(i)ne 1)..c(i,k)=g=c(i-1,k)+sum((j,s),(p3(i,j)/v(s))*x(i,j,k,s));
cs3(i,k)..c(i,k)=e=st(i,k)+sum((j,s),(p3(i,j)/v(s))*x(i,j,k,s));
mkspn3..cmax=g=c("2","3");
idl3(i)..teta(i)=e=cmax-sum((j,k,s),(p3(i,j)/v(s))*x(i,j,k,s));
enrg3..tec=e=sum((i,j,k,s),(pm*p3(i,j)*x(i,j,k,s)*landa(s))/(60*v(s)))+sum(i,(fi*pm*teta(i))/60);

option limcol=100;
option limrow=100;
option reslim=10000;
model mina1 /objtotal1,objcmax1,objtec1,posal1,jobal1,spdass1,c111,ck1,ci1,cs1,mkspn1,idl1,enrg1/;
model mina2 /objtotal2,objcmax2,objtec2,posal2,jobal2,spdass2,c112,ck2,ci2,cs2,mkspn2,idl2,enrg2/;
model mina3 /objtotal3,objcmax3,objtec3,posal3,jobal3,spdass3,c113,ck3,ci3,cs3,mkspn3,idl3,enrg3/;
loop(iter,
w=w-0.1
if(w>=0,
solve mina1 using mip minimization z1;
display w, z1.l, z11.l, z12.l, x.l;
solve mina2 using mip minimization z2;
display w, z2.l, z21.l, z22.l, x.l;
solve mina3 using mip minimization z3;
display w, z3.l, z31.l, z32.l, x.l;);
);


