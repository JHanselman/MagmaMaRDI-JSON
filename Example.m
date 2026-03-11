AttachSpec("spec");

file1:= "Testfiles/save_test.txt";
file2:= "Testfiles/save_test2.txt";
file3:= "Testfiles/save_test3.txt";
file4:= "Testfiles/save_test4.txt";
file5:= "Testfiles/save_test5.txt";
file6:= "Testfiles/save_test6.txt";
file7:= "Testfiles/save_test7.txt";
file8:= "Testfiles/save_test8.txt";
file9:= "Testfiles/save_test9.txt";
file10:= "Testfiles/save_test10.txt";
file11:= "Testfiles/save_test11.txt";
file12:= "Testfiles/save_test12.txt";
file13:= "Testfiles/save_test13.txt";
file14:= "Testfiles/save_test14.txt";
file15:= "Testfiles/save_test15.txt";
file16:= "Testfiles/save_test16.txt";
file17:= "Testfiles/save_test17.txt";
file18:= "Testfiles/save_test18.txt";

R<x,y,z> := PolynomialRing(Rationals(), 3);
p:= x^4*y^2 - 98*z^5 -23*x*y*z;
save_mardi_json(file1, p);

P<a,b> := PolynomialRing(R, 2);
save_mardi_json(file2, a*x+b*y);

save_mardi_json(file3, 3/5);

R<x> := PolynomialRing(Rationals());
p:= x^4- 32*x + 12/5;
save_mardi_json(file4, p);


K<a> := GF(13,5);
R<x> := PolynomialRing(K);
L, mL:= ext<K| x^2 -7>;
o:= L.1;
P<y,z> := PolynomialRing(L,2);
M := Matrix(P, 3,2, [o*y^2 -z,o^2+y*z,o-1, mL(a)^10, 4-z^4, 0]);
save_mardi_json(file5, M);


M := Matrix(Rationals(), 2, 3, [1,2,3,4,5,6]);
save_mardi_json(file6, M);

R<x,y,z> := PolynomialRing(Rationals(), 3);
M := Matrix(R, 3, 2, [0,0,0,z,x^2-1,y*z]);
save_mardi_json(file7, M);

K:= GF(11);
c := K!3;
save_mardi_json(file8, c);

R<x> := PolynomialRing(Rationals());
p:= x^4- 32*x + 12/5;
q := x^2 - 1;
save_mardi_json(file9, [p,q]);

save_mardi_json(file10, [1,2]);

R<x> := PolynomialRing(Rationals());
K<a> := NumberField(x^4+x^3+1);
save_mardi_json(file11, K);
save_mardi_json(file12, a^2-1);
S<t> := PolynomialRing(K);
L<b> := NumberField(t^2+31);
save_mardi_json(file13, L);
save_mardi_json(file14, b+8);


CC<I>:= ComplexField(50);
a:= Pi(CC);
save_mardi_json(file15, CC);
save_mardi_json(file16, a);

R<x,y> := PolynomialRing(Rationals(),2);
f := x^7 +2*x^3*y +y^3;
X := RiemannSurface(f);
tau := SmallPeriodMatrix(X);
save_mardi_json(file17, tau);


R<x> := PolynomialRing(Rationals());
K<a> := NumberField(x^4+x^3+1);
OK:= RingOfIntegers(K);
save_mardi_json(file18, OK);

AttachSpec("spec");
file19:= "Testfiles/save_test19.txt";
file20:= "Testfiles/save_test20.txt";
R<x> := PolynomialRing(Rationals());
K<a> := NumberField(x^4+x^3+1);
OK:= RingOfIntegers(K);
save_mardi_json(file19, OK!1);
save_mardi_json(file20, OK*1);

AttachSpec("spec");
file19:= "Testfiles/save_test19.txt";
load_mardi_json(file19);

load_mardi_json(file1);
load_mardi_json(file2);
load_mardi_json(file3);
load_mardi_json(file4);
load_mardi_json(file5);
load_mardi_json(file6);
load_mardi_json(file7);
load_mardi_json(file8);
load_mardi_json(file9);
load_mardi_json(file10);
load_mardi_json(file11);
load_mardi_json(file12);
load_mardi_json(file13);
load_mardi_json(file14);
load_mardi_json(file15);
load_mardi_json(file16);
load_mardi_json(file17);
load_mardi_json(file18);
