# !/bin/bash
rm -rf build
mkdir build
cd build
cmake ..
make
cd src
./gsm "int a,b = 3,2;
if a<6 :begin 
	a+=1;
end
elif a > 7 : begin
	a = 4;
end
else : begin
	a = 0;
end"
